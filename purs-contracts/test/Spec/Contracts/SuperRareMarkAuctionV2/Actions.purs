module Test.Spec.Contracts.SuperRareMarketAuctionV2.Actions where

import Prelude
import Chanterelle.Internal.Deploy (DeployReceipt)
import Chanterelle.Internal.Types (NoArgs)
import Contracts.SuperRareMarketAuctionV2 (acceptBid, bid, buy, cancelBid, currentBidDetailsOfToken, hasTokenBeenSold, markTokensAsSold, marketplaceFee, primarySaleFee, royaltyFee, setSalePrice, tokenPrice) as SuperRareMarketAuctionV2
import Contracts.TestExpensiveWallet as TestExpensiveWallet
import Data.Array (filter, length, zipWith)
import Data.Array.Partial (head)
import Data.Lens ((?~))
import Data.Ord (abs)
import Data.Symbol (SProxy(..))
import Data.Traversable (for, traverse)
import Deploy.Contracts.SuperRareV2 (SuperRareV2) as SuperRareV2
import Deploy.Utils (awaitTxSuccessWeb3)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
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
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Contracts.SuperRareV2 as SuperRareV2Spec
import Test.Spec.Contracts.Utils (defaultTxOpts, throwOnCallError, uInt256FromBigNumber)

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
    , testAssertFailOnPay :: DeployReceipt NoArgs
    , testExpensiveWallet :: DeployReceipt NoArgs
    , testRequireFailOnPay :: DeployReceipt NoArgs
    , testRevertOnPay :: DeployReceipt NoArgs
    | r
    }

-----------------------------------------------------------------------------
-- | cancelBid
-----------------------------------------------------------------------------
cancelBid ::
  forall r r1.
  TestEnv r ->
  { buyer :: Address
  , tokenId :: UIntN S256
  | r1
  } ->
  Web3 HexString
cancelBid tenv pd = do
  let
    { v2Marketplace: { deployAddress: marketContract }
    , v2SuperRare: { deployAddress: originContract }
    } = tenv

    { tokenId, buyer } = pd
  txHash <-
    SuperRareMarketAuctionV2.cancelBid
      ( defaultTxOpts buyer
          # _to
          ?~ marketContract
      )
      { _tokenId: tokenId, _originContract: originContract }
  awaitTxSuccessWeb3 txHash
  pure txHash

-----------------------------------------------------------------------------
-- | placeBid
-----------------------------------------------------------------------------
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

-----------------------------------------------------------------------------
-- | bid
-----------------------------------------------------------------------------
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

-----------------------------------------------------------------------------
-- | acceptBid
-----------------------------------------------------------------------------
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

-----------------------------------------------------------------------------
-- | mkTokensAndSetForSale
-----------------------------------------------------------------------------
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

-----------------------------------------------------------------------------
-- | setSalePrice
-----------------------------------------------------------------------------
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

-----------------------------------------------------------------------------
-- | genPriceAndSet
-----------------------------------------------------------------------------
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

-----------------------------------------------------------------------------
-- | marketplaceFee
-----------------------------------------------------------------------------
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

-----------------------------------------------------------------------------
-- | currentBidDetailsOfToken
-----------------------------------------------------------------------------
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

-----------------------------------------------------------------------------
-- | marketplaceFee
-----------------------------------------------------------------------------
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

-----------------------------------------------------------------------------
-- | primarySaleFee
-----------------------------------------------------------------------------
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

-----------------------------------------------------------------------------
-- | markTokensAsSold
-----------------------------------------------------------------------------
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

-----------------------------------------------------------------------------
-- | hasTokenBeenSold
-----------------------------------------------------------------------------
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

-----------------------------------------------------------------------------
-- | tokenPrice
-----------------------------------------------------------------------------
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

-----------------------------------------------------------------------------
-- | mkSuperRareTokens
-----------------------------------------------------------------------------
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

-----------------------------------------------------------------------------
-- | genTokenPrices
-----------------------------------------------------------------------------
genTokenPrices :: forall m. MonadEffect m => Int -> m (Array (UIntN S256))
genTokenPrices n =
  liftEffect (randomSample' n arbitrary)
    >>= traverse \(intPrice :: Int) ->
        let
          (unitPrice :: Value Szabo) = mkValue $ embed $ abs intPrice
        in
          pure $ uInt256FromBigNumber $ toMinorUnit unitPrice

-----------------------------------------------------------------------------
-- | mkPurchasePayload
-----------------------------------------------------------------------------
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

-----------------------------------------------------------------------------
-- | checkNewOwnerStatus
-----------------------------------------------------------------------------
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

-----------------------------------------------------------------------------
-- | checkEthDifference
-----------------------------------------------------------------------------
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
  log $ show { addr, diff, gasCostFactor: gasCostFactor * weiSpentOnGas }
  abs (balanceAfter - balanceBefore) `shouldEqual` diffWithGas
  where
  getBalance = eth_getBalance addr

  getTxDetails = do
    (Transaction { gasPrice, from, to }) <- eth_getTransaction txHash
    (TransactionReceipt { blockNumber, gasUsed }) <- eth_getTransactionReceipt txHash
    pure { blockNumber, gasPrice, gasUsed, from, to }

-----------------------------------------------------------------------------
-- | checkPayout
-----------------------------------------------------------------------------
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

-----------------------------------------------------------------------------
-- | buy
-----------------------------------------------------------------------------
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

-----------------------------------------------------------------------------
-- | expensiveWalletBid
-----------------------------------------------------------------------------
expensiveWalletBid ::
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
expensiveWalletBid tenv pd = do
  let
    { v2Marketplace: { deployAddress: marketContract }
    , v2SuperRare: { deployAddress: originContract }
    , testExpensiveWallet: { deployAddress: walletContract }
    , primaryAccount
    } = tenv

    { tokenId, price, buyerFee, buyer, owner } = pd
  asOwnerOfContract tenv buyer walletContract do
    txHash <-
      TestExpensiveWallet.bid
        ( defaultTxOpts buyer
            # _to
            ?~ walletContract
            # _value
            ?~ fromMinorUnit (price + buyerFee)
        )
        { _tokenId: tokenId
        , _originContract: originContract
        , _newBidAmount: uInt256FromBigNumber price
        , _market: marketContract
        }
    awaitTxSuccessWeb3 txHash
    pure txHash

-----------------------------------------------------------------------------
-- | claimMoneyFromExpensiveWallet
-----------------------------------------------------------------------------
claimMoneyFromExpensiveWallet ::
  forall r r1.
  TestEnv r ->
  { claimer :: Address
  | r1
  } ->
  Web3 HexString
claimMoneyFromExpensiveWallet tenv pd = do
  let
    { testExpensiveWallet: { deployAddress: walletContract }
    , v2SuperRare: { deployAddress: originContract }
    , v2Marketplace: { deployAddress: marketContract }
    , primaryAccount
    } = tenv

    { claimer } = pd
  asOwnerOfContract tenv claimer walletContract do
    txHash <-
      TestExpensiveWallet.claimMoney
        ( defaultTxOpts claimer
            # _to
            ?~ walletContract
        )
        { _escrowAddress: marketContract }
    awaitTxSuccessWeb3 txHash
    pure txHash

-----------------------------------------------------------------------------
-- | asOwnerOfContract
-----------------------------------------------------------------------------
asOwnerOfContract ::
  forall r a. { primaryAccount :: Address | r } -> Address -> Address -> Web3 a -> Web3 a
asOwnerOfContract { primaryAccount } tmpOwner cAddr f = do
  TestExpensiveWallet.transferOwnership
    ( defaultTxOpts primaryAccount
        # _to
        ?~ cAddr
    )
    { newOwner: tmpOwner }
    >>= awaitTxSuccessWeb3
  res <- f
  TestExpensiveWallet.transferOwnership
    ( defaultTxOpts tmpOwner
        # _to
        ?~ cAddr
    )
    { newOwner: primaryAccount }
    >>= awaitTxSuccessWeb3
  pure res
