module Test.Spec.Contracts.SuperRareMarketAuctionV2 where

import Prelude
import Chanterelle.Test (buildTestConfig)
import Data.Array (filter, length, replicate, zipWith)
import Data.Array.Partial (head)
import Data.Maybe (fromJust)
import Data.Symbol (SProxy(..))
import Data.Traversable (for)
import Deploy.Contracts.SuperRareMarketAuctionV2 (deployScript) as SuperRareMarketAuctionV2
import Deploy.Contracts.TestContracts (deployScript) as TestContracts
import Effect.Aff (Aff)
import Network.Ethereum.Core.HexString (nullWord, takeHex)
import Network.Ethereum.Web3 (embed, mkAddress, unUIntN)
import Partial.Unsafe (unsafePartial)
import Record as Record
import Test.Spec (SpecT, beforeAll, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Contracts.SuperRareLegacy as SuperRareLegacySpec
import Test.Spec.Contracts.SuperRareMarketAuctionV2.Actions (TestEnv, acceptBid, assertFailBid, bid, buy, cancelBid, checkEthDifference, checkNewOwnerStatus, checkPayout, claimMoneyFromExpensiveWallet, currentBidDetailsOfToken, expensiveWalletBid, genPercentageLessThan, genPriceAndSet, genTokenPrices, hasTokenBeenSold, markTokensAsSold, mkPurchasePayload, mkSuperRareTokens, mkTokensAndSetForSale, payments, placeBid, requireFailBid, revertFailBid, safeAcceptBid, safeBuy, setERC721ContractRoyaltyFee, setSalePrice, tokenPrice)
import Test.Spec.Contracts.SuperRareV2 as SuperRareV2Spec
import Test.Spec.Contracts.Utils (intToUInt256, uInt256FromBigNumber, web3Test)

spec :: SpecT Aff Unit Aff Unit
spec =
  beforeAll init do
    describe "SuperRareMarketAuctionV2" do
      it "can mark tokens as sold" \tenv@{ provider } ->
        web3Test provider do
          newTokens <- mkSuperRareTokens tenv 1
          markTokensAsSold tenv (newTokens <#> \{ tokenId } -> tokenId)
          isMarkedSolds <-
            for (newTokens <#> \{ tokenId } -> tokenId) (hasTokenBeenSold tenv)
          isMarkedSolds `shouldEqual` replicate (length newTokens) true
      it "can set the price of tokens" \tenv@{ provider } ->
        web3Test provider do
          newTokens <- mkSuperRareTokens tenv 1
          prices <- genTokenPrices $ length newTokens
          let
            tokenDetails = zipWith (Record.insert (SProxy :: _ "price")) prices newTokens
          void
            $ for tokenDetails \td@{ tokenId, price, owner } -> do
                setSalePrice tenv owner tokenId price
          onChainPrices <-
            for tokenDetails \{ tokenId } -> tokenPrice tenv tokenId
          onChainPrices `shouldEqual` (tokenDetails <#> \{ price } -> price)
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
            $ for purchaseRess \{ tokenId, buyer: owner, uri } -> do
                price <- genPriceAndSet tenv owner tokenId
                purchasePayloads <- mkPurchasePayload tenv { tokenId, owner, price, uri }
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
            $ for purchaseRess \{ tokenId, buyer: owner, uri } -> do
                price <- genPriceAndSet tenv owner tokenId
                purchasePayloads <- mkPurchasePayload tenv { tokenId, owner, price, uri }
                let
                  buyer = unsafePartial head $ filter (\acc -> acc /= owner) accounts

                  updatedPayload = Record.disjointUnion { buyer } purchasePayloads
                safeBuy tenv updatedPayload
      it "can place a bid" \tenv@{ provider, accounts, v2Marketplace: { deployAddress: marketAddr } } ->
        web3Test provider do
          tokenDetails <- mkSuperRareTokens tenv 1
          prices <- map unUIntN <$> genTokenPrices (length tokenDetails)
          let
            tokensAndBids = zipWith (Record.insert (SProxy :: _ "price")) prices tokenDetails
          bidRess <- for tokensAndBids (placeBid tenv)
          currentBids <- for bidRess (\{ tokenId } -> currentBidDetailsOfToken tenv tokenId)
          currentBids `shouldEqual` (bidRess <#> \{ price, buyer } -> { price: uInt256FromBigNumber price, bidder: buyer })
          void
            $ for bidRess \bidsRes@{ buyer, buyerFee, purchaseTxHash, price } -> do
                checkEthDifference buyer (price + buyerFee) purchaseTxHash
                checkEthDifference marketAddr (price + buyerFee) purchaseTxHash
      it "can place a bid outbidding another" \tenv@{ provider, accounts, v2Marketplace: { deployAddress: marketAddr } } ->
        web3Test provider do
          tokenDetails <- mkSuperRareTokens tenv 1
          prices <- map unUIntN <$> genTokenPrices (length tokenDetails)
          let
            tokensAndBids = zipWith (Record.insert (SProxy :: _ "price")) prices tokenDetails
          bidRess <- for tokensAndBids (placeBid tenv)
          bidRess2 <-
            for bidRess \pb@{ price, buyer, tokenId, uri, owner, buyerFee } -> do
              purch <- mkPurchasePayload tenv { owner, tokenId, price: price * embed 2, uri }
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
                (\price { buyer, tokenId, uri } -> { price, owner: buyer, tokenId, uri })
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
                (\price { buyer, tokenId, uri } -> { price, owner: buyer, tokenId, uri })
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
          currentBids <- for cancelBidRess (\{ tokenId } -> currentBidDetailsOfToken tenv tokenId)
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
            for bidRess \pb@{ price, buyer, tokenId, uri, owner, buyerFee } -> do
              purch <-
                mkPurchasePayload
                  tenv
                  { owner, tokenId, price: price * embed 2, uri }
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
            $ for bidRess \pb@{ price, buyer, tokenId, uri, owner, buyerFee } -> do
                purch <-
                  mkPurchasePayload
                    tenv
                    { owner, tokenId, price: price * embed 2, uri }
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
            $ for bidRess \pb@{ price, buyer, tokenId, uri, owner, buyerFee } -> do
                purch <-
                  mkPurchasePayload
                    tenv
                    { owner, tokenId, price: price * embed 2, uri }
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
            $ for bidRess \pb@{ price, buyer, tokenId, uri, owner, buyerFee } -> do
                purch <-
                  mkPurchasePayload
                    tenv
                    { owner, tokenId, price: price * embed 2, uri }
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
          void $ setERC721ContractRoyaltyFee tenv _originContract newPerencetage
          bidRess <- for tokensAndBids (placeBid tenv)
          acceptRess <-
            for bidRess \abPayload -> do
              txHash <- acceptBid tenv abPayload
              pure abPayload { purchaseTxHash = txHash }
          pricesSec <- map unUIntN <$> genTokenPrices (length acceptRess)
          let
            tokensAndBidsSec =
              zipWith
                (\price { buyer, tokenId, uri } -> { price, owner: buyer, tokenId, uri })
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

-----------------------------------------------------------------------------
-- | Init
-----------------------------------------------------------------------------
init :: Aff (TestEnv ())
init = do
  tenv@{ provider, primaryAccount } <- initSupeRareV2
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
        }
        tenv
  where
  initSupeRareV2 = do
    tenv@{ accounts, provider } <- SuperRareV2Spec.init
    web3Test provider $ whitelistAddresses tenv
    pure tenv

  initSupeRareLegacy = SuperRareLegacySpec.init

  whitelistAddresses tenv@{ accounts } = void $ for accounts (SuperRareV2Spec.whitelistAddress tenv)

  approveMarketplace tenv@{ accounts } marketplace =
    void
      $ for accounts (\acc -> SuperRareV2Spec.setApprovalForAll tenv acc marketplace true)
