module Test.Spec.Contracts.SuperRareMarketAuctionV2 where

import Prelude
import Chanterelle.Test (buildTestConfig)
import Data.Array (filter, length, replicate, zipWith)
import Data.Array.Partial (head)
import Data.Either (isLeft)
import Data.Lens ((?~))
import Data.Maybe (Maybe(..), fromJust)
import Data.Symbol (SProxy(..))
import Data.Traversable (for)
import Deploy.Contracts.SuperRareMarketAuctionV2 (deployScript) as SuperRareMarketAuctionV2
import Deploy.Contracts.TestContracts (deployScript) as TestContracts
import Effect.Aff (Aff, try)
import Network.Ethereum.Core.HexString (nullWord, takeHex)
import Network.Ethereum.Web3 (_to, embed, mkAddress, unUIntN)
import Partial.Unsafe (unsafePartial)
import Record as Record
import Test.Spec (SpecT, beforeAll, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.Contracts.SupeRare as SupeRare
import Test.Spec.Contracts.SuperRareLegacy as SuperRareLegacySpec
import Test.Spec.Contracts.SuperRareLegacy.Actions as SuperRareLegacy
import Test.Spec.Contracts.SuperRareMarketAuctionV2.Actions (TestEnv, acceptBid, assertFailBid, bid, buy, cancelBid, checkEthDifference, checkNewOwnerStatus, checkPayout, claimMoneyFromExpensiveWallet, currentBidDetailsOfToken, expensiveWalletBid, genPercentageLessThan, genPriceAndSet, genTokenPrices, hasTokenBeenSold, markTokensAsSold, mkPurchasePayload, mkSuperRareTokens, mkTokensAndSetForSale, payments, placeBid, requireFailBid, revertFailBid, safeAcceptBid, safeBuy, setERC721ContractRoyaltySettings, setSalePrice, tokenPrice)
import Test.Spec.Contracts.SuperRareV2 as SuperRareV2
import Test.Spec.Contracts.SuperRareV2 as SuperRareV2Spec
import Test.Spec.Contracts.Utils (defaultTxOpts, intToUInt256, uInt256FromBigNumber, web3Test)

spec :: SpecT Aff Unit Aff Unit
spec =
  beforeAll init do
    describe "SuperRareMarketAuctionV2" do
      it "can mark tokens as sold" \tenv@{ provider, v2SuperRare: { deployAddress: srV2Addr } } ->
        web3Test provider do
          newTokens <- mkSuperRareTokens tenv 1
          markTokensAsSold tenv srV2Addr (newTokens <#> \{ tokenId } -> tokenId)
          void
            $ for newTokens \{ tokenId, contractAddress } -> do
                isSold <- hasTokenBeenSold tenv contractAddress tokenId
                isSold `shouldEqual` true
      it "can set the price of tokens" \tenv@{ provider } ->
        web3Test provider do
          newTokens <- mkSuperRareTokens tenv 1
          prices <- genTokenPrices $ length newTokens
          let
            tokenDetails = zipWith (Record.insert (SProxy :: _ "price")) prices newTokens
          void
            $ for tokenDetails \td@{ tokenId, contractAddress, price, owner } -> do
                setSalePrice tenv contractAddress owner tokenId price
                onChainPrice <- tokenPrice tenv contractAddress tokenId
                onChainPrice `shouldEqual` price
      it "cannot set the price of token if not approved" \tenv@{ provider } ->
        web3Test provider do
          newTokens <- mkSuperRareTokens tenv 1
          prices <- genTokenPrices $ length newTokens
          let
            { v2Marketplace: { deployAddress } } = tenv

            tokenDetails = zipWith (Record.insert (SProxy :: _ "price")) prices newTokens
          void
            $ for tokenDetails \td@{ tokenId, contractAddress, price, owner } -> do
                SuperRareV2.setApprovalForAll tenv owner deployAddress false
                eres <- try $ setSalePrice tenv contractAddress owner tokenId price
                eres `shouldSatisfy` isLeft
                SuperRareV2.setApprovalForAll tenv owner deployAddress true
      it "cannot set the price of token if not token owner" \tenv@{ provider } ->
        web3Test provider do
          newTokens <- mkSuperRareTokens tenv 1
          prices <- genTokenPrices $ length newTokens
          let
            { v2Marketplace: { deployAddress }, accounts } = tenv

            tokenDetails = zipWith (Record.insert (SProxy :: _ "price")) prices newTokens
          void
            $ for tokenDetails \td@{ tokenId, contractAddress, price, owner } -> do
                let
                  nonOwner = unsafePartial head $ filter (\a -> a /= owner) accounts
                eres <- try $ setSalePrice tenv contractAddress nonOwner tokenId price
                eres `shouldSatisfy` isLeft
      it "cannot make sale if token is not for sale" \tenv@{ provider, accounts } ->
        web3Test provider do
          tokenDetails <- mkSuperRareTokens tenv 1
          void
            $ for tokenDetails \td@{ tokenId, owner } -> do
                purchasePayload <- mkPurchasePayload tenv (Record.insert (SProxy :: _ "price") (embed 100000000) td)
                let
                  buyer = unsafePartial head $ filter (\acc -> acc /= owner) accounts

                  updatedPayload = Record.disjointUnion { buyer } purchasePayload
                eres <- try $ buy tenv updatedPayload
                eres `shouldSatisfy` isLeft
      it "cannot make sale if owner no longer has marketplace approved" \tenv@{ provider, accounts } ->
        web3Test provider do
          tokenDetails <- mkTokensAndSetForSale tenv 1
          void
            $ for tokenDetails \td@{ tokenId, price, owner } -> do
                purchasePayload <- mkPurchasePayload tenv td
                let
                  buyer = unsafePartial head $ filter (\acc -> acc /= owner) accounts

                  { v2Marketplace: { deployAddress }, accounts } = tenv

                  updatedPayload = Record.disjointUnion { buyer } purchasePayload
                SuperRareV2.setApprovalForAll tenv owner deployAddress false
                eres <- try $ buy tenv updatedPayload
                eres `shouldSatisfy` isLeft
                SuperRareV2.setApprovalForAll tenv owner deployAddress true
      it "cannot make sale if price setter no longer owns the token" \tenv@{ provider, accounts } ->
        web3Test provider do
          tokenDetails <- mkTokensAndSetForSale tenv 1
          void
            $ for tokenDetails \td@{ tokenId, price, owner } -> do
                purchasePayload <- mkPurchasePayload tenv td
                let
                  buyer = unsafePartial head $ filter (\acc -> acc /= owner) accounts

                  newOwner = unsafePartial head $ filter (\acc -> acc /= owner && acc /= buyer) accounts

                  { v2Marketplace: { deployAddress }, accounts } = tenv

                  updatedPayload = Record.disjointUnion { buyer } purchasePayload
                SuperRareV2.transferFrom tenv owner owner newOwner tokenId
                eres <- try $ buy tenv updatedPayload
                eres `shouldSatisfy` isLeft
      it "cannot make sale if price is not correct" \tenv@{ provider, accounts } ->
        web3Test provider do
          tokenDetails <- mkTokensAndSetForSale tenv 1
          void
            $ for tokenDetails \td@{ tokenId, price, owner } -> do
                purchasePayload <- mkPurchasePayload tenv td
                let
                  buyer = unsafePartial head $ filter (\acc -> acc /= owner) accounts

                  newOwner = unsafePartial head $ filter (\acc -> acc /= owner && acc /= buyer) accounts

                  { v2Marketplace: { deployAddress }, accounts } = tenv

                  newPrice = sub price (embed 1000)

                  updatedPayload = Record.disjointUnion { buyer } purchasePayload
                SuperRareV2.transferFrom tenv owner owner newOwner tokenId
                eres <- try $ buy tenv updatedPayload { price = newPrice }
                eres `shouldSatisfy` isLeft
      it "can make sale - primary" \tenv@{ provider, accounts } ->
        web3Test provider do
          tokenDetails <- mkTokensAndSetForSale tenv 1
          void
            $ for tokenDetails \td@{ tokenId, price, owner } -> do
                purchasePayload <- mkPurchasePayload tenv td
                let
                  buyer = unsafePartial head $ filter (\acc -> acc /= owner) accounts

                  updatedPayload = Record.disjointUnion { buyer } purchasePayload
                purchaseRes <- buy tenv updatedPayload
                checkNewOwnerStatus tenv purchaseRes
                checkPayout purchaseRes
      it "can make sale - secondary" \tenv@{ provider, accounts } -> do
        web3Test provider do
          tokenDetails <- mkTokensAndSetForSale tenv 1
          purchaseRess <-
            for tokenDetails \td@{ tokenId, price, owner } -> do
              purchasePayload <- mkPurchasePayload tenv td
              let
                buyer = unsafePartial head $ filter (\acc -> acc /= owner) accounts

                updatedPayload = Record.disjointUnion { buyer } purchasePayload
              buy tenv updatedPayload
          void
            $ for purchaseRess \{ tokenId, buyer: owner, uri, contractAddress } -> do
                price <- genPriceAndSet tenv contractAddress owner tokenId
                purchasePayloads <-
                  mkPurchasePayload tenv
                    { tokenId
                    , owner
                    , price
                    , uri
                    , contractAddress
                    }
                let
                  buyer = unsafePartial head $ filter (\acc -> acc /= owner) accounts

                  updatedPayload = Record.disjointUnion { buyer } purchasePayloads
                buy tenv updatedPayload
      it "can make safeSale - primary" \tenv@{ provider, accounts } ->
        web3Test provider do
          tokenDetails <- mkTokensAndSetForSale tenv 1
          void
            $ for tokenDetails \td@{ tokenId, price, owner } -> do
                purchasePayload <- mkPurchasePayload tenv td
                let
                  buyer = unsafePartial head $ filter (\acc -> acc /= owner) accounts

                  updatedPayload = Record.disjointUnion { buyer } purchasePayload
                purchaseRes <- safeBuy tenv updatedPayload
                checkNewOwnerStatus tenv purchaseRes
                checkPayout purchaseRes
      it "can make safeSale - secondary" \tenv@{ provider, accounts } -> do
        web3Test provider do
          tokenDetails <- mkTokensAndSetForSale tenv 1
          purchaseRess <-
            for tokenDetails \td@{ tokenId, price, owner } -> do
              purchasePayload <- mkPurchasePayload tenv td
              let
                buyer = unsafePartial head $ filter (\acc -> acc /= owner) accounts

                updatedPayload = Record.disjointUnion { buyer } purchasePayload
              safeBuy tenv updatedPayload
          void
            $ for purchaseRess \{ tokenId, buyer: owner, uri, contractAddress } -> do
                price <- genPriceAndSet tenv contractAddress owner tokenId
                purchasePayloads <-
                  mkPurchasePayload tenv
                    { tokenId
                    , owner
                    , price
                    , uri
                    , contractAddress
                    }
                let
                  buyer = unsafePartial head $ filter (\acc -> acc /= owner) accounts

                  updatedPayload = Record.disjointUnion { buyer } purchasePayloads
                safeBuy tenv updatedPayload
      it "cannot bid 0" \tenv@{ provider, accounts, v2Marketplace: { deployAddress: marketAddr } } ->
        web3Test provider do
          tokenDetails <- mkSuperRareTokens tenv 1
          let
            prices = replicate (length tokenDetails) (embed 0)

            tokensAndBids = zipWith (Record.insert (SProxy :: _ "price")) prices tokenDetails
          void
            $ for tokensAndBids \td -> do
                eres <- try $ placeBid tenv td
                eres `shouldSatisfy` isLeft
      it "can place a bid" \tenv@{ provider, accounts, v2Marketplace: { deployAddress: marketAddr } } ->
        web3Test provider do
          tokenDetails <- mkSuperRareTokens tenv 1
          prices <- map unUIntN <$> genTokenPrices (length tokenDetails)
          let
            tokensAndBids = zipWith (Record.insert (SProxy :: _ "price")) prices tokenDetails
          bidRess <- for tokensAndBids (placeBid tenv)
          currentBids <- for bidRess (\{ tokenId, contractAddress } -> currentBidDetailsOfToken tenv contractAddress tokenId)
          currentBids `shouldEqual` (bidRess <#> \{ price, buyer } -> { price: uInt256FromBigNumber price, bidder: buyer })
          void
            $ for bidRess \bidsRes@{ buyer, buyerFee, purchaseTxHash, price } -> do
                checkEthDifference buyer (price + buyerFee) purchaseTxHash
                checkEthDifference marketAddr (price + buyerFee) purchaseTxHash
      it "cannot place a bid lower than previous bid" \tenv@{ provider, accounts, v2Marketplace: { deployAddress: marketAddr } } ->
        web3Test provider do
          tokenDetails <- mkSuperRareTokens tenv 1
          prices <- map unUIntN <$> genTokenPrices (length tokenDetails)
          let
            tokensAndBids = zipWith (Record.insert (SProxy :: _ "price")) prices tokenDetails
          bidRess <- for tokensAndBids (placeBid tenv)
          void
            $ for bidRess \pb@{ price, buyer, tokenId, uri, owner, buyerFee, contractAddress } -> do
                purch <-
                  mkPurchasePayload tenv
                    { owner
                    , tokenId
                    , price: price `sub` embed 1
                    , uri
                    , contractAddress
                    }
                let
                  newBuyer = unsafePartial head $ filter (\acc -> acc /= owner && acc /= buyer) accounts

                  updatedPayload = (Record.disjointUnion purch { buyer: newBuyer })
                eres <- try $ bid tenv updatedPayload
                eres `shouldSatisfy` isLeft
      it "can place a bid outbidding another" \tenv@{ provider, accounts, v2Marketplace: { deployAddress: marketAddr } } ->
        web3Test provider do
          tokenDetails <- mkSuperRareTokens tenv 1
          prices <- map unUIntN <$> genTokenPrices (length tokenDetails)
          let
            tokensAndBids = zipWith (Record.insert (SProxy :: _ "price")) prices tokenDetails
          bidRess <- for tokensAndBids (placeBid tenv)
          bidRess2 <-
            for bidRess \pb@{ price, buyer, tokenId, uri, owner, buyerFee, contractAddress } -> do
              purch <-
                mkPurchasePayload tenv
                  { owner
                  , tokenId
                  , price: price * embed 2
                  , uri
                  , contractAddress
                  }
              let
                newBuyer = unsafePartial head $ filter (\acc -> acc /= owner && acc /= buyer) accounts

                updatedPayload = (Record.disjointUnion purch { buyer: newBuyer })
              purchaseTxHash <- bid tenv updatedPayload
              pure
                $ Record.disjointUnion updatedPayload
                    { purchaseTxHash
                    , oldPrice: price
                    , oldBidder: buyer
                    , oldBuyerFee: buyerFee
                    }
          void
            $ for bidRess2 \bidRes -> do
                let
                  { buyer
                  , buyerFee
                  , purchaseTxHash
                  , price
                  , oldBuyerFee
                  , oldBidder
                  , oldPrice
                  } = bidRes
                checkEthDifference oldBidder (oldPrice + oldBuyerFee) purchaseTxHash
                checkEthDifference buyer (price + buyerFee) purchaseTxHash
                checkEthDifference marketAddr (price + buyerFee - (oldPrice + oldBuyerFee)) purchaseTxHash
      it "can accept a bid - primary" \tenv@{ provider, accounts, v2Marketplace: { deployAddress: marketAddr } } ->
        web3Test provider do
          tokenDetails <- mkSuperRareTokens tenv 1
          prices <- map unUIntN <$> genTokenPrices (length tokenDetails)
          let
            tokensAndBids = zipWith (Record.insert (SProxy :: _ "price")) prices tokenDetails
          bidRess <- for tokensAndBids (placeBid tenv)
          acceptRess <-
            for bidRess \abPayload -> do
              txHash <- acceptBid tenv abPayload
              pure abPayload { purchaseTxHash = txHash }
          void
            $ for acceptRess \pd@{ owner, sellerFee, purchaseTxHash, price } -> do
                checkNewOwnerStatus tenv pd
                checkEthDifference owner (price - sellerFee) purchaseTxHash
      it "can accept a bid - seconday" \tenv@{ provider, accounts, v2Marketplace: { deployAddress: marketAddr } } ->
        web3Test provider do
          tokenDetails <- mkSuperRareTokens tenv 1
          prices <- map unUIntN <$> genTokenPrices (length tokenDetails)
          let
            tokensAndBids = zipWith (Record.insert (SProxy :: _ "price")) prices tokenDetails
          bidRess <- for tokensAndBids (placeBid tenv)
          acceptRess <-
            for bidRess \abPayload -> do
              txHash <- acceptBid tenv abPayload
              pure abPayload { purchaseTxHash = txHash }
          pricesSec <- map unUIntN <$> genTokenPrices (length acceptRess)
          let
            tokensAndBidsSec =
              zipWith
                (\price { buyer, tokenId, uri, contractAddress } -> { price, owner: buyer, tokenId, uri, contractAddress })
                pricesSec
                acceptRess
          bidRessSec <- for tokensAndBidsSec (placeBid tenv)
          acceptRessSec <-
            for bidRessSec \abPayload -> do
              txHash <- acceptBid tenv abPayload
              pure abPayload { purchaseTxHash = txHash }
          void
            $ for acceptRessSec \pd@{ owner, sellerFee, purchaseTxHash, price } -> do
                checkNewOwnerStatus tenv pd
                checkEthDifference owner (price - sellerFee) purchaseTxHash
      it "can safe accept a bid - primary" \tenv@{ provider, accounts, v2Marketplace: { deployAddress: marketAddr } } ->
        web3Test provider do
          tokenDetails <- mkSuperRareTokens tenv 1
          prices <- map unUIntN <$> genTokenPrices (length tokenDetails)
          let
            tokensAndBids = zipWith (Record.insert (SProxy :: _ "price")) prices tokenDetails
          bidRess <- for tokensAndBids (placeBid tenv)
          acceptRess <-
            for bidRess \abPayload -> do
              txHash <- safeAcceptBid tenv abPayload
              pure abPayload { purchaseTxHash = txHash }
          void
            $ for acceptRess \pd@{ owner, sellerFee, purchaseTxHash, price } -> do
                checkNewOwnerStatus tenv pd
                checkEthDifference owner (price - sellerFee) purchaseTxHash
      it "can accept a bid - seconday" \tenv@{ provider, accounts, v2Marketplace: { deployAddress: marketAddr } } ->
        web3Test provider do
          tokenDetails <- mkSuperRareTokens tenv 1
          prices <- map unUIntN <$> genTokenPrices (length tokenDetails)
          let
            tokensAndBids = zipWith (Record.insert (SProxy :: _ "price")) prices tokenDetails
          bidRess <- for tokensAndBids (placeBid tenv)
          acceptRess <-
            for bidRess \abPayload -> do
              txHash <- safeAcceptBid tenv abPayload
              pure abPayload { purchaseTxHash = txHash }
          pricesSec <- map unUIntN <$> genTokenPrices (length acceptRess)
          let
            tokensAndBidsSec =
              zipWith
                (\price { buyer, tokenId, uri, contractAddress } -> { price, owner: buyer, tokenId, uri, contractAddress })
                pricesSec
                acceptRess
          bidRessSec <- for tokensAndBidsSec (placeBid tenv)
          acceptRessSec <-
            for bidRessSec \abPayload -> do
              txHash <- safeAcceptBid tenv abPayload
              pure abPayload { purchaseTxHash = txHash }
          void
            $ for acceptRessSec \pd@{ owner, sellerFee, purchaseTxHash, price } -> do
                checkNewOwnerStatus tenv pd
                checkEthDifference owner (price - sellerFee) purchaseTxHash
      it "can cancel a bid and release funds" \tenv@{ provider, accounts, v2Marketplace: { deployAddress: marketAddr } } ->
        web3Test provider do
          tokenDetails <- mkSuperRareTokens tenv 1
          prices <- map unUIntN <$> genTokenPrices (length tokenDetails)
          let
            tokensAndBids = zipWith (Record.insert (SProxy :: _ "price")) prices tokenDetails

            zeroAddress = unsafePartial fromJust $ mkAddress $ takeHex 40 nullWord
          bidRess <- for tokensAndBids (placeBid tenv)
          cancelBidRess <-
            for bidRess \br -> do
              txHash <- cancelBid tenv br
              pure br { purchaseTxHash = txHash }
          currentBids <- for cancelBidRess (\{ tokenId, contractAddress } -> currentBidDetailsOfToken tenv contractAddress tokenId)
          currentBids `shouldEqual` replicate (length cancelBidRess) { price: intToUInt256 0, bidder: zeroAddress }
          void
            $ for bidRess \{ buyer, buyerFee, purchaseTxHash, price } -> do
                checkEthDifference buyer (price + buyerFee) purchaseTxHash
                checkEthDifference marketAddr (price + buyerFee) purchaseTxHash
      it "can escrow if returning funds on bid fails due to gas, funds can be reclaimed " \tenv@{ provider } ->
        web3Test provider do
          let
            { accounts, v2Marketplace: { deployAddress: marketAddr }, testExpensiveWallet: { deployAddress: walletAddr } } = tenv
          tokenDetails <- mkSuperRareTokens tenv 1
          prices <- map unUIntN <$> genTokenPrices (length tokenDetails)
          let
            tokensAndBids = zipWith (Record.insert (SProxy :: _ "price")) prices tokenDetails
          bidRess <-
            for tokensAndBids \tb@{ owner } -> do
              purch <- mkPurchasePayload tenv tb
              let
                buyer = unsafePartial head $ filter (\acc -> acc /= owner) accounts

                updatedPayload = (Record.disjointUnion purch { buyer })
              purchaseTxHash <- expensiveWalletBid tenv updatedPayload
              pure $ Record.disjointUnion updatedPayload { purchaseTxHash }
          bidRess2 <-
            for bidRess \pb@{ price, buyer, tokenId, uri, owner, buyerFee, contractAddress } -> do
              purch <-
                mkPurchasePayload
                  tenv
                  { owner, tokenId, price: price * embed 2, uri, contractAddress }
              let
                newBuyer =
                  unsafePartial head
                    $ filter (\acc -> acc /= owner && acc /= buyer) accounts

                updatedPayload = (Record.disjointUnion purch { buyer: newBuyer })
              purchaseTxHash <- bid tenv updatedPayload
              owedPayment <- unUIntN <$> payments tenv walletAddr
              owedPayment `shouldEqual` (price + buyerFee)
              checkEthDifference buyer (embed 0) purchaseTxHash
              checkEthDifference marketAddr owedPayment purchaseTxHash
              pure
                $ Record.disjointUnion updatedPayload
                    { purchaseTxHash
                    , oldPrice: price
                    , oldBidder: buyer
                    , oldBuyerFee: buyerFee
                    }
          void
            $ for bidRess2 \br@{ price, buyer, tokenId, uri, owner, buyerFee, oldBidder, oldPrice, oldBuyerFee } -> do
                txHash <- claimMoneyFromExpensiveWallet tenv { claimer: oldBidder }
                checkEthDifference oldBidder (oldPrice + oldBuyerFee) txHash
      it "can escrow if returning funds fails on assertion" \tenv@{ provider } ->
        web3Test provider do
          let
            { accounts, v2Marketplace: { deployAddress: marketAddr }, testAssertFailOnPay: { deployAddress: assertfailOnPayAddr } } = tenv
          tokenDetails <- mkSuperRareTokens tenv 1
          prices <- map unUIntN <$> genTokenPrices (length tokenDetails)
          let
            tokensAndBids = zipWith (Record.insert (SProxy :: _ "price")) prices tokenDetails
          bidRess <-
            for tokensAndBids \tb@{ owner } -> do
              purch <- mkPurchasePayload tenv tb
              let
                buyer = unsafePartial head $ filter (\acc -> acc /= owner) accounts

                updatedPayload = (Record.disjointUnion purch { buyer })
              purchaseTxHash <- assertFailBid tenv updatedPayload
              pure $ Record.disjointUnion updatedPayload { purchaseTxHash }
          void
            $ for bidRess \pb@{ price, buyer, tokenId, uri, owner, buyerFee, contractAddress } -> do
                purch <-
                  mkPurchasePayload
                    tenv
                    { owner, tokenId, price: price * embed 2, uri, contractAddress }
                let
                  newBuyer =
                    unsafePartial head
                      $ filter (\acc -> acc /= owner && acc /= buyer) accounts

                  updatedPayload = (Record.disjointUnion purch { buyer: newBuyer })
                purchaseTxHash <- bid tenv updatedPayload
                owedPayment <- unUIntN <$> payments tenv assertfailOnPayAddr
                checkEthDifference buyer (embed 0) purchaseTxHash
                owedPayment `shouldEqual` (price + buyerFee)
      it "can escrow if returning funds fails on require" \tenv@{ provider } ->
        web3Test provider do
          let
            { accounts
            , v2Marketplace:
                { deployAddress: marketAddr }
            , testRequireFailOnPay: { deployAddress: requirefailOnPayAddr }
            } = tenv
          tokenDetails <- mkSuperRareTokens tenv 1
          prices <- map unUIntN <$> genTokenPrices (length tokenDetails)
          let
            tokensAndBids = zipWith (Record.insert (SProxy :: _ "price")) prices tokenDetails
          bidRess <-
            for tokensAndBids \tb@{ owner } -> do
              purch <- mkPurchasePayload tenv tb
              let
                buyer = unsafePartial head $ filter (\acc -> acc /= owner) accounts

                updatedPayload = (Record.disjointUnion purch { buyer })
              purchaseTxHash <- requireFailBid tenv updatedPayload
              pure $ Record.disjointUnion updatedPayload { purchaseTxHash }
          void
            $ for bidRess \pb@{ price, buyer, tokenId, uri, owner, buyerFee, contractAddress } -> do
                purch <-
                  mkPurchasePayload
                    tenv
                    { owner, tokenId, price: price * embed 2, uri, contractAddress }
                let
                  newBuyer =
                    unsafePartial head
                      $ filter (\acc -> acc /= owner && acc /= buyer) accounts

                  updatedPayload = (Record.disjointUnion purch { buyer: newBuyer })
                purchaseTxHash <- bid tenv updatedPayload
                owedPayment <- unUIntN <$> payments tenv requirefailOnPayAddr
                checkEthDifference buyer (embed 0) purchaseTxHash
      it "can escrow if returning funds fails on revert" \tenv@{ provider } ->
        web3Test provider do
          let
            { accounts
            , v2Marketplace:
                { deployAddress: marketAddr }
            , testRevertOnPay: { deployAddress: revertfailOnPayAddr }
            } = tenv
          tokenDetails <- mkSuperRareTokens tenv 1
          prices <- map unUIntN <$> genTokenPrices (length tokenDetails)
          let
            tokensAndBids = zipWith (Record.insert (SProxy :: _ "price")) prices tokenDetails
          bidRess <-
            for tokensAndBids \tb@{ owner } -> do
              purch <- mkPurchasePayload tenv tb
              let
                buyer = unsafePartial head $ filter (\acc -> acc /= owner) accounts

                updatedPayload = (Record.disjointUnion purch { buyer })
              purchaseTxHash <- revertFailBid tenv updatedPayload
              pure $ Record.disjointUnion updatedPayload { purchaseTxHash }
          void
            $ for bidRess \pb@{ price, buyer, tokenId, uri, owner, buyerFee, contractAddress } -> do
                purch <-
                  mkPurchasePayload
                    tenv
                    { owner, tokenId, price: price * embed 2, uri, contractAddress }
                let
                  newBuyer =
                    unsafePartial head
                      $ filter (\acc -> acc /= owner && acc /= buyer) accounts

                  updatedPayload = (Record.disjointUnion purch { buyer: newBuyer })
                purchaseTxHash <- bid tenv updatedPayload
                owedPayment <- unUIntN <$> payments tenv revertfailOnPayAddr
                checkEthDifference buyer (embed 0) purchaseTxHash
                owedPayment `shouldEqual` (price + buyerFee)
      it "can modify the royalty percentage and accept a bid with appropriate royalty" \tenv@{ provider } ->
        web3Test provider do
          tokenDetails <- mkSuperRareTokens tenv 1
          newPerencetage <- genPercentageLessThan 20
          prices <- map unUIntN <$> genTokenPrices (length tokenDetails)
          let
            { accounts
            , v2Marketplace: { deployAddress: marketAddr }
            , v2SuperRare: { deployAddress: _originContract }
            } = tenv

            tokensAndBids = zipWith (Record.insert (SProxy :: _ "price")) prices tokenDetails
          void $ setERC721ContractRoyaltySettings tenv _originContract _originContract newPerencetage
          bidRess <- for tokensAndBids (placeBid tenv)
          acceptRess <-
            for bidRess \abPayload -> do
              txHash <- acceptBid tenv abPayload
              pure abPayload { purchaseTxHash = txHash }
          pricesSec <- map unUIntN <$> genTokenPrices (length acceptRess)
          let
            tokensAndBidsSec =
              zipWith
                (\price { buyer, tokenId, uri, contractAddress } -> { price, owner: buyer, tokenId, uri, contractAddress })
                pricesSec
                acceptRess
          bidRessSec <- for tokensAndBidsSec (placeBid tenv)
          acceptRessSec <-
            for bidRessSec \abPayload -> do
              txHash <- acceptBid tenv abPayload
              pure abPayload { purchaseTxHash = txHash }
          void
            $ for acceptRessSec \pd@{ owner, sellerFee, purchaseTxHash, price } -> do
                checkNewOwnerStatus tenv pd
                checkEthDifference owner (price - sellerFee) purchaseTxHash
      it "should place bid on token before being upgraded, have it upgrade, and then have bid be accepted" \tenv@{ provider } ->
        web3Test provider do
          let
            { accounts
            , superRareLegacy: { deployAddress: legacyAddr }
            , v2Marketplace:
                { deployAddress: marketAddr
                }
            } = tenv

            legacyCallArgs =
              ( defaultTxOpts legacyAddr
                  # _to
                  ?~ legacyAddr
              )

            tid = intToUInt256 2
          uri <- SupeRare.tokenURI tenv tid
          owner <- SupeRare.ownerOf tenv tid
          prices <- map unUIntN <$> genTokenPrices 1
          let
            tokensAndBids = zipWith (Record.insert (SProxy :: _ "price")) prices [ { owner, uri, tokenId: tid, contractAddress: legacyAddr } ]
          bidRess <- for tokensAndBids (placeBid tenv)
          void $ SuperRareLegacy.setApprovalForAll tenv owner marketAddr true
          void $ for bidRess $ \{ tokenId, owner: owner' } -> SuperRareLegacy.upgrade tenv owner' tokenId
          void
            $ for bidRess \abPayload -> do
                txHash <- safeAcceptBid tenv abPayload
                pure abPayload { purchaseTxHash = txHash }

-----------------------------------------------------------------------------
-- | Init
-----------------------------------------------------------------------------
init :: Aff (TestEnv ())
init = do
  tenv@{ provider, supeRare, accounts, primaryAccount } <- initSupeRareV2
  let
    srTenv = { supeRare, provider, accounts, primaryAccount }
  { superRareLegacy, numOldSuperRareTokens } <- initSupeRareLegacy srTenv
  { superRareMarketAuctionV2 } <-
    buildTestConfig "http://localhost:8545" 60
      SuperRareMarketAuctionV2.deployScript
  { testAssertFailOnPay
  , testExpensiveWallet
  , testRequireFailOnPay
  , testRevertOnPay
  } <-
    buildTestConfig "http://localhost:8545" 60
      TestContracts.deployScript
  web3Test provider
    $ approveMarketplace tenv superRareMarketAuctionV2.deployAddress
  pure
    $ Record.merge
        { v2Marketplace: superRareMarketAuctionV2
        , testAssertFailOnPay
        , testExpensiveWallet
        , testRequireFailOnPay
        , testRevertOnPay
        , superRareLegacy
        , numOldSuperRareTokens
        }
        tenv
  where
  initSupeRareV2 = do
    tenv@{ accounts, provider } <- SuperRareV2Spec.init
    web3Test provider $ whitelistAddresses tenv
    pure tenv

  initSupeRareLegacy = SuperRareLegacySpec.init <<< Just

  whitelistAddresses tenv@{ accounts } = void $ for accounts (SuperRareV2Spec.whitelistAddress tenv)

  approveMarketplace tenv@{ accounts } marketplace =
    void
      $ for accounts (\acc -> SuperRareV2Spec.setApprovalForAll tenv acc marketplace true)
