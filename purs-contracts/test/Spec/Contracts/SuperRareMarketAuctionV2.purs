module Test.Spec.Contracts.SuperRareMarketAuctionV2 where

import Prelude
import Chanterelle.Internal.Deploy (DeployReceipt)
import Chanterelle.Internal.Types (NoArgs)
import Chanterelle.Test (buildTestConfig)
import Contracts.SuperRareMarketAuctionV2 (acceptBid, bid, buy, currentBidDetailsOfToken, hasTokenBeenSold, markTokensAsSold, marketplaceFee, primarySaleFee, royaltyFee, setSalePrice, tokenPrice) as SuperRareMarketAuctionV2
import Data.Array (filter, length, replicate, zipWith)
import Data.Array.Partial (head)
import Data.Lens ((?~))
import Data.Ord (abs)
import Data.Symbol (SProxy(..))
import Data.Traversable (for, traverse)
import Deploy.Contracts.SuperRareMarketAuctionV2 (deployScript) as SuperRareMarketAuctionV2
import Deploy.Contracts.SuperRareV2 (SuperRareV2) as SuperRareV2
import Deploy.Utils (awaitTxSuccessWeb3)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Network.Ethereum.Core.BigNumber (divide)
import Network.Ethereum.Web3 (Address, BigNumber, BlockNumber(..), ChainCursor(..), HexString, Provider, Szabo, Transaction(..), TransactionReceipt(..), UIntN, Value, Web3, _to, _value, embed, fromMinorUnit, mkValue, toMinorUnit, unUIntN)
import Network.Ethereum.Web3.Api (eth_getBalance, eth_getTransaction, eth_getTransactionReceipt)
import Network.Ethereum.Web3.Solidity (Tuple2(..))
import Network.Ethereum.Web3.Solidity.Sizes (S256)
import Partial.Unsafe (unsafePartial)
import Prim.Row (class Lacks)
import Record as Record
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (randomSample')
import Test.Spec (SpecT, beforeAll, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Contracts.SuperRareV2 as SuperRareV2Spec
import Test.Spec.Contracts.Utils (defaultTxOpts, throwOnCallError, uInt256FromBigNumber, web3Test)

spec :: SpecT Aff Unit Aff Unit
spec =
  beforeAll init do
    describe "SuperRareMarketAuctionV2" do
      it "can mark tokens as sold" \tenv@{ provider } ->
        web3Test provider do
          newTokens <- mkSuperRareTokens tenv 4
          markTokensAsSold tenv (newTokens <#> \{ tokenId } -> tokenId)
          isMarkedSolds <-
            for (newTokens <#> \{ tokenId } -> tokenId) (hasTokenBeenSold tenv)
          isMarkedSolds `shouldEqual` replicate (length newTokens) true
      it "can set the price of tokens" \tenv@{ provider } ->
        web3Test provider do
          newTokens <- mkSuperRareTokens tenv 4
          prices <- genTokenPrices $ length newTokens
          let
            tokenDetails = zipWith (Record.insert (SProxy :: _ "price")) prices newTokens
          void
            $ for tokenDetails \td@{ tokenId, price, owner } -> do
                setSalePrice tenv owner tokenId price
          onChainPrices <-
            for tokenDetails \{ tokenId } -> tokenPrice tenv tokenId
          onChainPrices `shouldEqual` (tokenDetails <#> \{ price } -> price)
      it "can make sale primary" \tenv@{ provider, accounts } ->
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

-----------------------------------------------------------------------------
-- | TestEnv
-----------------------------------------------------------------------------
type TestEnv r
  = { supeRare :: DeployReceipt NoArgs
    , provider :: Provider
    , accounts :: Array Address
    , primaryAccount :: Address
    , v2SuperRare :: DeployReceipt SuperRareV2.SuperRareV2
    , v2Marketplace :: DeployReceipt NoArgs
    | r
    }

init :: Aff (TestEnv ())
init = do
  tenv@{ provider } <- initSupeRareV2
  { superRareMarketAuctionV2 } <-
    buildTestConfig "http://localhost:8545" 60
      SuperRareMarketAuctionV2.deployScript
  web3Test provider
    $ approveMarketplace tenv superRareMarketAuctionV2.deployAddress
  pure $ Record.insert (SProxy :: _ "v2Marketplace") superRareMarketAuctionV2 tenv
  where
  initSupeRareV2 = do
    tenv@{ accounts, provider } <- SuperRareV2Spec.init
    web3Test provider $ whitelistAddresses tenv
    pure tenv

  whitelistAddresses tenv@{ accounts } = void $ for accounts (SuperRareV2Spec.whitelistAddress tenv)

  approveMarketplace tenv@{ accounts } marketplace =
    void
      $ for accounts (\acc -> SuperRareV2Spec.setApprovalForAll tenv acc marketplace true)

placeBid ::
  forall r.
  TestEnv r ->
  { tokenId :: UIntN S256
  , owner :: Address
  , price :: BigNumber
  , uri :: String
  } ->
  Web3
    { tokenId :: UIntN S256
    , owner :: Address
    , price :: BigNumber
    , buyerFee :: BigNumber
    , sellerFee :: BigNumber
    , purchaseTxHash :: HexString
    , buyer :: Address
    , uri :: String
    }
placeBid tenv@{ accounts } td@{ owner, tokenId, price, uri } = do
  purch <- mkPurchasePayload tenv { owner, tokenId, price, uri }
  let
    buyer = unsafePartial head $ filter (\acc -> acc /= owner) accounts

    updatedPayload = (Record.disjointUnion purch { buyer })
  purchaseTxHash <- bid tenv updatedPayload
  pure $ Record.disjointUnion updatedPayload { purchaseTxHash }

bid ::
  forall r r1.
  TestEnv r ->
  { buyer :: Address
  , tokenId :: UIntN S256
  , owner :: Address
  , price :: BigNumber
  , buyerFee :: BigNumber
  | r1
  } ->
  Web3 HexString
bid tenv pd = do
  let
    { v2Marketplace: { deployAddress: marketContract }
    , v2SuperRare: { deployAddress: originContract }
    } = tenv

    { tokenId, price, buyerFee, buyer, owner } = pd
  txHash <-
    SuperRareMarketAuctionV2.bid
      ( defaultTxOpts buyer
          # _to
          ?~ marketContract
          # _value
          ?~ fromMinorUnit (price + buyerFee)
      )
      { _tokenId: tokenId, _originContract: originContract, _newBidAmount: uInt256FromBigNumber price }
  awaitTxSuccessWeb3 txHash
  pure txHash

acceptBid ::
  forall r r1.
  TestEnv r ->
  { buyer :: Address
  , tokenId :: UIntN S256
  , owner :: Address
  | r1
  } ->
  Web3 HexString
acceptBid tenv pd = do
  let
    { v2Marketplace: { deployAddress: marketContract }
    , v2SuperRare: { deployAddress: originContract }
    } = tenv

    { tokenId, owner } = pd
  txHash <-
    SuperRareMarketAuctionV2.acceptBid
      ( defaultTxOpts owner
          # _to
          ?~ marketContract
      )
      { _tokenId: tokenId, _originContract: originContract }
  awaitTxSuccessWeb3 txHash
  pure txHash

mkTokensAndSetForSale ::
  forall r.
  TestEnv r ->
  Int ->
  Web3
    ( Array { owner :: Address, price :: BigNumber, tokenId :: UIntN S256, uri :: String }
    )
mkTokensAndSetForSale tenv n = do
  newTokens <- mkSuperRareTokens tenv n
  prices <- genTokenPrices $ length newTokens
  marketfee <- marketplaceFee tenv
  let
    tokenDetails = zipWith (Record.insert (SProxy :: _ "price")) prices newTokens
  updatedDetails <-
    for tokenDetails \td@{ tokenId, price, owner, uri } -> do
      setSalePrice tenv owner tokenId price
      pure { owner, tokenId, price: unUIntN price, uri }
  pure updatedDetails

setSalePrice ::
  forall r. TestEnv r -> Address -> UIntN S256 -> UIntN S256 -> Web3 Unit
setSalePrice tenv owner _tokenId _amount =
  let
    { v2SuperRare: { deployAddress: _originContract }
    , v2Marketplace: { deployAddress }
    , primaryAccount
    } = tenv
  in
    SuperRareMarketAuctionV2.setSalePrice
      (defaultTxOpts owner # _to ?~ deployAddress)
      { _originContract, _tokenId, _amount }
      >>= awaitTxSuccessWeb3

genPriceAndSet ::
  forall r. TestEnv r -> Address -> UIntN S256 -> Web3 BigNumber
genPriceAndSet tenv owner tokenId = do
  let
    { v2SuperRare: { deployAddress: _originContract }
    , v2Marketplace: { deployAddress }
    , primaryAccount
    } = tenv
  do
    price <- unsafePartial head <$> genTokenPrices 1
    setSalePrice tenv owner tokenId price
    pure $ unUIntN price

marketplaceFee ::
  forall r. TestEnv r -> Web3 (UIntN S256)
marketplaceFee tenv =
  let
    { v2Marketplace: { deployAddress }
    , primaryAccount
    } = tenv
  in
    throwOnCallError
      $ SuperRareMarketAuctionV2.marketplaceFee
          (defaultTxOpts primaryAccount # _to ?~ deployAddress)
          Latest

currentBidDetailsOfToken ::
  forall r. TestEnv r -> UIntN S256 -> Web3 { price :: UIntN S256, bidder :: Address }
currentBidDetailsOfToken tenv _tokenId = do
  let
    { v2Marketplace: { deployAddress }
    , v2SuperRare: { deployAddress: _originContract }
    , primaryAccount
    } = tenv
  (Tuple2 price bidder) <-
    throwOnCallError
      $ SuperRareMarketAuctionV2.currentBidDetailsOfToken
          (defaultTxOpts primaryAccount # _to ?~ deployAddress)
          Latest
          { _originContract, _tokenId }
  pure { price, bidder }

royaltyFee ::
  forall r. TestEnv r -> Web3 (UIntN S256)
royaltyFee tenv =
  let
    { v2Marketplace: { deployAddress }
    , primaryAccount
    } = tenv
  in
    throwOnCallError
      $ SuperRareMarketAuctionV2.royaltyFee
          (defaultTxOpts primaryAccount # _to ?~ deployAddress)
          Latest

primarySaleFee ::
  forall r. TestEnv r -> Web3 (UIntN S256)
primarySaleFee tenv =
  let
    { v2Marketplace: { deployAddress }
    , primaryAccount
    } = tenv
  in
    throwOnCallError
      $ SuperRareMarketAuctionV2.primarySaleFee
          (defaultTxOpts primaryAccount # _to ?~ deployAddress)
          Latest

markTokensAsSold ::
  forall r. TestEnv r -> Array (UIntN S256) -> Web3 Unit
markTokensAsSold tenv _tokenIds =
  let
    { v2SuperRare: { deployAddress: _originContract }
    , v2Marketplace: { deployAddress }
    , primaryAccount
    } = tenv
  in
    SuperRareMarketAuctionV2.markTokensAsSold
      (defaultTxOpts primaryAccount # _to ?~ deployAddress)
      { _originContract, _tokenIds }
      >>= awaitTxSuccessWeb3

hasTokenBeenSold ::
  forall r. TestEnv r -> UIntN S256 -> Web3 Boolean
hasTokenBeenSold tenv _tokenId =
  let
    { v2SuperRare: { deployAddress: _originContract }
    , v2Marketplace: { deployAddress }
    , primaryAccount
    } = tenv
  in
    throwOnCallError
      $ SuperRareMarketAuctionV2.hasTokenBeenSold
          (defaultTxOpts primaryAccount # _to ?~ deployAddress)
          Latest
          { _originContract, _tokenId }

tokenPrice ::
  forall r. TestEnv r -> UIntN S256 -> Web3 (UIntN S256)
tokenPrice tenv _tokenId =
  let
    { v2SuperRare: { deployAddress: _originContract }
    , v2Marketplace: { deployAddress }
    , primaryAccount
    } = tenv
  in
    throwOnCallError
      $ SuperRareMarketAuctionV2.tokenPrice
          (defaultTxOpts primaryAccount # _to ?~ deployAddress)
          Latest
          { _originContract, _tokenId }

mkSuperRareTokens ::
  forall r.
  TestEnv r ->
  Int ->
  Web3 (Array { owner ∷ Address, tokenId ∷ UIntN S256, uri ∷ String })
mkSuperRareTokens tenv n =
  SuperRareV2Spec.createTokensWithFunction
    tenv
    n
    (SuperRareV2Spec.addNewToken tenv)

genTokenPrices :: forall m. MonadEffect m => Int -> m (Array (UIntN S256))
genTokenPrices n =
  liftEffect (randomSample' n arbitrary)
    >>= traverse \(intPrice :: Int) ->
        let
          (unitPrice :: Value Szabo) = mkValue $ embed $ abs intPrice
        in
          pure $ uInt256FromBigNumber $ toMinorUnit unitPrice

mkPurchasePayload ::
  forall r.
  TestEnv r ->
  { owner :: Address
  , price :: BigNumber
  , tokenId :: UIntN S256
  , uri :: String
  } ->
  Web3
    { buyerFee :: BigNumber
    , owner :: Address
    , price :: BigNumber
    , sellerFee :: BigNumber
    , tokenId :: UIntN S256
    , uri :: String
    }
mkPurchasePayload tenv td = do
  let
    { tokenId, price, owner } = td
  marketfee <- unUIntN <$> marketplaceFee tenv
  royaltyfee <- unUIntN <$> royaltyFee tenv
  primfee <- unUIntN <$> primarySaleFee tenv
  sold <- hasTokenBeenSold tenv tokenId
  let
    sellerFeePercent =
      if not sold then
        primfee
      else
        royaltyfee

    sellerFee = divide (price * sellerFeePercent) (embed 100)

    buyerFee = divide (price * marketfee) (embed 100)
  pure $ Record.merge { buyerFee, sellerFee } td

checkNewOwnerStatus ::
  forall r r1.
  TestEnv r ->
  { buyer :: Address
  , tokenId :: UIntN S256
  | r1
  } ->
  Web3 Unit
checkNewOwnerStatus tenv { buyer, tokenId } = do
  owner <- SuperRareV2Spec.ownerOf tenv tokenId
  buyer `shouldEqual` owner

checkEthDifference :: Address -> BigNumber -> HexString -> Web3 Unit
checkEthDifference addr diff txHash = do
  { blockNumber, gasUsed, gasPrice, from, to } <- getTxDetails
  let
    (BlockNumber blockNumBN) = blockNumber

    weiSpentOnGas = gasPrice * gasUsed
  balanceAfter <- getBalance $ BN blockNumber
  balanceBefore <- getBalance $ BN $ BlockNumber $ blockNumBN - embed 1
  let
    gasCostFactor = if balanceAfter > balanceBefore then embed (-1) else embed 1

    diffWithGas = diff + if from == addr then gasCostFactor * weiSpentOnGas else embed 0
  abs (balanceAfter - balanceBefore) `shouldEqual` diffWithGas
  where
  getBalance = eth_getBalance addr

  getTxDetails = do
    (Transaction { gasPrice, from, to }) <- eth_getTransaction txHash
    (TransactionReceipt { blockNumber, gasUsed }) <- eth_getTransactionReceipt txHash
    pure { blockNumber, gasPrice, gasUsed, from, to }

checkPayout ::
  forall r.
  { buyer :: Address
  , owner :: Address
  , buyerFee :: BigNumber
  , price :: BigNumber
  , purchaseTxHash :: HexString
  , sellerFee :: BigNumber
  | r
  } ->
  Web3 Unit
checkPayout { buyer, owner, purchaseTxHash, price, buyerFee, sellerFee } = do
  checkEthDifference buyer (buyerFee + price) purchaseTxHash
  checkEthDifference owner (price - sellerFee) purchaseTxHash

buy ::
  forall r r1.
  Lacks "purchaseTxHash" r1 =>
  TestEnv r ->
  { buyer :: Address
  , tokenId :: UIntN S256
  , owner :: Address
  , price :: BigNumber
  , buyerFee :: BigNumber
  | r1
  } ->
  Web3
    { buyer :: Address
    , tokenId :: UIntN S256
    , owner :: Address
    , price :: BigNumber
    , buyerFee :: BigNumber
    , purchaseTxHash :: HexString
    | r1
    }
buy tenv pd = do
  let
    { v2Marketplace: { deployAddress: marketContract }
    , v2SuperRare: { deployAddress: originContract }
    } = tenv

    { tokenId, price, buyerFee, buyer, owner } = pd
  txHash <-
    SuperRareMarketAuctionV2.buy
      ( defaultTxOpts buyer
          # _to
          ?~ marketContract
          # _value
          ?~ fromMinorUnit (price + buyerFee)
      )
      { _tokenId: tokenId, _originContract: originContract }
  awaitTxSuccessWeb3 txHash
  pure $ Record.insert (SProxy :: _ "purchaseTxHash") txHash pd
 {-
    deploy SR AuctionMarketV2
    -- 
    -- buy
    -- bid
    -- accept bid
    deploy hack
    -- make hack bid on V2 Marketplace
    -- outbid hack bid on V2 Marketplace
    -- 
    -- 
    -- 
        
        
-}