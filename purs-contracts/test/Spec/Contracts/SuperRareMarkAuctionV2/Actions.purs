module Test.Spec.Contracts.SuperRareMarketAuctionV2.Actions where

import Prelude
import Chanterelle.Internal.Deploy (DeployReceipt)
import Chanterelle.Internal.Types (NoArgs)
import Contracts.Marketplace.IMarketplaceSettings as IMarketplaceSettings
import Contracts.Marketplace.MarketplaceSettings as MarketplaceSettings
import Contracts.SuperRareMarketAuctionV2 (acceptBid, bid, buy, cancelBid, currentBidDetailsOfToken, iMarketplaceSettings, payments, safeAcceptBid, safeBuy, setSalePrice, tokenPrice) as SuperRareMarketAuctionV2
import Contracts.SuperRareRoyaltyRegistry (getERC721TokenRoyaltyPercentage)
import Contracts.TestAssertFailOnPay as TestAssertFailOnPay
import Contracts.TestExpensiveWallet as TestExpensiveWallet
import Contracts.TestRequireFailOnPay as TestRequireFailOnPay
import Contracts.TestRevertOnPay as TestRevertFailOnPay
import Control.Monad.Error.Class (class MonadError, catchError, throwError)
import Data.Array (filter, length, zipWith)
import Data.Array.Partial (head)
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Data.Ord (abs)
import Data.Symbol (SProxy(..))
import Data.Traversable (for, traverse)
import Deploy.Contracts.SuperRareLegacy (SuperRareLegacy)
import Deploy.Contracts.SuperRareMarketAuctionV2 (SuperRareMarketAuctionV2)
import Deploy.Contracts.SuperRareV2 (SuperRareV2) as SuperRareV2
import Deploy.Utils (awaitTxSuccessWeb3)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error, error, message)
import Network.Ethereum.Core.BigNumber (decimal, divide, parseBigNumber)
import Network.Ethereum.Web3 (Address, BigNumber, BlockNumber(..), ChainCursor(..), HexString, Provider, Szabo, Transaction(..), TransactionReceipt(..), UIntN, Value, Web3, _gas, _to, _value, embed, fromMinorUnit, mkValue, toMinorUnit, unUIntN)
import Network.Ethereum.Web3.Api (eth_getBalance, eth_getTransaction, eth_getTransactionReceipt)
import Network.Ethereum.Web3.Solidity (Tuple2(..))
import Network.Ethereum.Web3.Solidity.Sizes (S256, S8)
import Partial.Unsafe (unsafePartial)
import Prim.Row (class Lacks)
import Record as Record
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (chooseInt, randomSample')
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Contracts.SuperRareV2 as SuperRareV2Spec
import Test.Spec.Contracts.Utils (createTokensWithFunction, defaultTxOpts, throwOnCallError, uInt256FromBigNumber)

-----------------------------------------------------------------------------
-- | TestEnv
-----------------------------------------------------------------------------
type TestEnv r
  = { supeRare :: DeployReceipt NoArgs
    , provider :: Provider
    , accounts :: Array Address
    , primaryAccount :: Address
    , v2SuperRare :: DeployReceipt SuperRareV2.SuperRareV2
    , v2Marketplace :: DeployReceipt SuperRareMarketAuctionV2
    , superRareLegacy :: DeployReceipt SuperRareLegacy
    , numOldSuperRareTokens :: Int
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
  , contractAddress :: Address
  | r1
  } ->
  Web3 HexString
cancelBid tenv pd = do
  let
    { v2Marketplace: { deployAddress: marketContract }
    } = tenv

    { tokenId, contractAddress, buyer } = pd
  txHash <-
    SuperRareMarketAuctionV2.cancelBid
      ( defaultTxOpts buyer
          # _to
          ?~ marketContract
      )
      { _tokenId: tokenId, _originContract: contractAddress }
  awaitTxSuccessWeb3 txHash
  pure txHash

-----------------------------------------------------------------------------
-- | placeBid
-----------------------------------------------------------------------------
placeBid ::
  forall r.
  TestEnv r ->
  { tokenId :: UIntN S256
  , contractAddress :: Address
  , owner :: Address
  , price :: BigNumber
  , uri :: String
  } ->
  Web3
    { tokenId :: UIntN S256
    , contractAddress :: Address
    , owner :: Address
    , price :: BigNumber
    , buyerFee :: BigNumber
    , sellerFee :: BigNumber
    , purchaseTxHash :: HexString
    , buyer :: Address
    , uri :: String
    }
placeBid tenv@{ accounts } td@{ owner, tokenId, price, uri, contractAddress } = do
  purch <- mkPurchasePayload tenv { owner, tokenId, price, uri, contractAddress }
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
  , contractAddress :: Address
  , owner :: Address
  , price :: BigNumber
  , buyerFee :: BigNumber
  | r1
  } ->
  Web3 HexString
bid tenv pd = do
  let
    { v2Marketplace: { deployAddress: marketContract }
    } = tenv

    { tokenId, price, buyerFee, buyer, owner, contractAddress } = pd
  txHash <-
    SuperRareMarketAuctionV2.bid
      ( defaultTxOpts buyer
          # _to
          ?~ marketContract
          # _value
          ?~ fromMinorUnit (price + buyerFee)
      )
      { _tokenId: tokenId, _originContract: contractAddress, _newBidAmount: uInt256FromBigNumber price }
  awaitTxSuccessWeb3 txHash
  pure txHash

-----------------------------------------------------------------------------
-- | safeAcceptBid
-----------------------------------------------------------------------------
safeAcceptBid ::
  forall r r1.
  TestEnv r ->
  { buyer :: Address
  , tokenId :: UIntN S256
  , contractAddress :: Address
  , owner :: Address
  , price :: BigNumber
  | r1
  } ->
  Web3 HexString
safeAcceptBid tenv pd = do
  let
    { v2Marketplace: { deployAddress: marketContract }
    } = tenv

    { tokenId, owner, price, contractAddress } = pd
  txHash <-
    SuperRareMarketAuctionV2.safeAcceptBid
      ( defaultTxOpts owner
          # _to
          ?~ marketContract
      )
      { _tokenId: tokenId, _originContract: contractAddress, _amount: uInt256FromBigNumber price }
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
  , contractAddress :: Address
  , owner :: Address
  | r1
  } ->
  Web3 HexString
acceptBid tenv pd = do
  let
    { v2Marketplace: { deployAddress: marketContract }
    } = tenv

    { tokenId, contractAddress, owner } = pd
  txHash <-
    SuperRareMarketAuctionV2.acceptBid
      ( defaultTxOpts owner
          # _to
          ?~ marketContract
      )
      { _tokenId: tokenId, _originContract: contractAddress }
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
    ( Array
        { owner :: Address
        , price :: BigNumber
        , tokenId :: UIntN S256
        , contractAddress :: Address
        , uri :: String
        }
    )
mkTokensAndSetForSale tenv n = do
  newTokens <- mkSuperRareTokens tenv n
  prices <- genTokenPrices $ length newTokens
  marketfee <- marketplaceFee tenv
  let
    tokenDetails = zipWith (Record.insert (SProxy :: _ "price")) prices newTokens
  updatedDetails <-
    for tokenDetails \td@{ tokenId, price, owner, uri, contractAddress } -> do
      setSalePrice tenv contractAddress owner tokenId price
      pure { owner, tokenId, price: unUIntN price, uri, contractAddress }
  pure updatedDetails

-----------------------------------------------------------------------------
-- | setSalePrice
-----------------------------------------------------------------------------
setSalePrice ::
  forall r.
  TestEnv r ->
  Address -> Address -> UIntN S256 -> UIntN S256 -> Web3 Unit
setSalePrice tenv _originContract owner _tokenId _amount =
  let
    { v2Marketplace: { deployAddress }
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
  forall r. TestEnv r -> Address -> Address -> UIntN S256 -> Web3 BigNumber
genPriceAndSet tenv contractAddress owner tokenId = do
  let
    { v2Marketplace: { deployAddress }
    , primaryAccount
    } = tenv
  do
    price <- unsafePartial head <$> genTokenPrices 1
    setSalePrice tenv contractAddress owner tokenId price
    pure $ unUIntN price

-----------------------------------------------------------------------------
-- | marketplaceFee
-----------------------------------------------------------------------------
marketplaceFee ::
  forall r. TestEnv r -> Web3 (UIntN S8)
marketplaceFee tenv = do
  let
    { v2Marketplace: { deployAddress }
    , primaryAccount
    } = tenv
  ims <-
    throwOnCallError
      $ SuperRareMarketAuctionV2.iMarketplaceSettings
          (defaultTxOpts primaryAccount # _to ?~ deployAddress)
          Latest
  throwOnCallError
    $ IMarketplaceSettings.getMarketplaceFeePercentage
        (defaultTxOpts primaryAccount # _to ?~ ims)
        Latest

-----------------------------------------------------------------------------
-- | currentBidDetailsOfToken
-----------------------------------------------------------------------------
currentBidDetailsOfToken ::
  forall r. TestEnv r -> Address -> UIntN S256 -> Web3 { price :: UIntN S256, bidder :: Address }
currentBidDetailsOfToken tenv _originContract _tokenId = do
  let
    { v2Marketplace: { deployAddress }
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
-- | getTokenRoyaltyPercentage
-----------------------------------------------------------------------------
getTokenRoyaltyPercentage ::
  forall r. TestEnv r -> Address -> UIntN S256 -> Web3 (UIntN S8)
getTokenRoyaltyPercentage tenv _contractAddress _tokenId = do
  let
    { v2Marketplace: { deployAddress }
    , primaryAccount
    } = tenv
  throwOnCallError
    $ getERC721TokenRoyaltyPercentage
        (defaultTxOpts primaryAccount # _to ?~ deployAddress)
        Latest
        { _contractAddress, _tokenId }

-----------------------------------------------------------------------------
-- | getERC721ContractPrimarySaleFee
-----------------------------------------------------------------------------
getERC721ContractPrimarySaleFee ::
  forall r. TestEnv r -> Address -> Web3 (UIntN S8)
getERC721ContractPrimarySaleFee tenv _contractAddress = do
  let
    { v2Marketplace: { deployAddress }
    , primaryAccount
    } = tenv
  ims <-
    throwOnCallError
      $ SuperRareMarketAuctionV2.iMarketplaceSettings
          (defaultTxOpts primaryAccount # _to ?~ deployAddress)
          Latest
  throwOnCallError
    $ IMarketplaceSettings.getERC721ContractPrimarySaleFeePercentage
        (defaultTxOpts primaryAccount # _to ?~ ims)
        Latest
        { _contractAddress }

-----------------------------------------------------------------------------
-- | markTokensAsSold
-----------------------------------------------------------------------------
markTokensAsSold ::
  forall r. TestEnv r -> Address -> Array (UIntN S256) -> Web3 Unit
markTokensAsSold tenv _originContract _tokenIds = do
  let
    { v2Marketplace: { deployAddress }
    , primaryAccount
    } = tenv
  ims <-
    throwOnCallError
      $ SuperRareMarketAuctionV2.iMarketplaceSettings
          (defaultTxOpts primaryAccount # _to ?~ deployAddress)
          Latest
  MarketplaceSettings.markTokensAsSold
    (defaultTxOpts primaryAccount # _to ?~ ims)
    { _originContract, _tokenIds }
    >>= awaitTxSuccessWeb3

-----------------------------------------------------------------------------
-- | hasTokenBeenSold
-----------------------------------------------------------------------------
hasTokenBeenSold ::
  forall r. TestEnv r -> Address -> UIntN S256 -> Web3 Boolean
hasTokenBeenSold tenv _contractAddress _tokenId = do
  let
    { v2Marketplace: { deployAddress }
    , primaryAccount
    } = tenv
  ims <-
    throwOnCallError
      $ SuperRareMarketAuctionV2.iMarketplaceSettings
          (defaultTxOpts primaryAccount # _to ?~ deployAddress)
          Latest
  throwOnCallError
    $ IMarketplaceSettings.hasERC721TokenSold
        (defaultTxOpts primaryAccount # _to ?~ ims)
        Latest
        { _contractAddress, _tokenId }

-----------------------------------------------------------------------------
-- | tokenPrice
-----------------------------------------------------------------------------
tokenPrice ::
  forall r. TestEnv r -> Address -> UIntN S256 -> Web3 (UIntN S256)
tokenPrice tenv _originContract _tokenId =
  let
    { v2Marketplace: { deployAddress }
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
  Web3 (Array { owner ∷ Address, tokenId ∷ UIntN S256, uri ∷ String, contractAddress :: Address })
mkSuperRareTokens tenv n = do
  createTokensWithFunction
    tenv
    n
    (SuperRareV2Spec.addNewToken tenv)

-----------------------------------------------------------------------------
-- | genTokenPrices
-----------------------------------------------------------------------------
genPercentageLessThan :: forall m. MonadEffect m => Int -> m (UIntN S256)
genPercentageLessThan n =
  liftEffect (randomSample' 1 $ chooseInt 1 n)
    >>= \v -> pure $ uInt256FromBigNumber $ embed $ unsafePartial $ head v

-----------------------------------------------------------------------------
-- | genPercentage
-----------------------------------------------------------------------------
genTokenPrices :: forall m. MonadEffect m => Int -> m (Array (UIntN S256))
genTokenPrices n =
  liftEffect (randomSample' n arbitrary)
    >>= traverse \(intPrice :: Int) ->
        let
          (unitPrice :: Value Szabo) = mkValue $ embed $ abs intPrice
        in
          pure $ uInt256FromBigNumber $ (*) (embed 10) $ toMinorUnit unitPrice

-----------------------------------------------------------------------------
-- | mkPurchasePayload
-----------------------------------------------------------------------------
mkPurchasePayload ::
  forall r.
  TestEnv r ->
  { owner :: Address
  , price :: BigNumber
  , tokenId :: UIntN S256
  , contractAddress :: Address
  , uri :: String
  } ->
  Web3
    { buyerFee :: BigNumber
    , owner :: Address
    , price :: BigNumber
    , sellerFee :: BigNumber
    , tokenId :: UIntN S256
    , contractAddress :: Address
    , uri :: String
    }
mkPurchasePayload tenv td = do
  let
    { tokenId, contractAddress, price, owner } = td
  marketfee <- unUIntN <$> marketplaceFee tenv
  royaltyfee <- getTokenRoyaltyPercentage tenv contractAddress tokenId
  primfee <- unUIntN <$> getERC721ContractPrimarySaleFee tenv contractAddress
  sold <- hasTokenBeenSold tenv contractAddress tokenId
  let
    sellerFeePercent =
      if not sold then
        primfee
      else
        unUIntN royaltyfee

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
  assertWithContext { gasUsed, diffWithGas, balanceBefore, balanceAfter, txHash }
    $ abs (balanceAfter - balanceBefore) `shouldEqual` diffWithGas
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
checkPayout p@{ buyer, owner, purchaseTxHash, price, buyerFee, sellerFee } = do
  checkEthDifference buyer (buyerFee + price) purchaseTxHash
  checkEthDifference owner (price - sellerFee) purchaseTxHash

-----------------------------------------------------------------------------
-- | safeBuy
-----------------------------------------------------------------------------
safeBuy ::
  forall r r1.
  Lacks "purchaseTxHash" r1 =>
  TestEnv r ->
  { buyer :: Address
  , tokenId :: UIntN S256
  , contractAddress :: Address
  , owner :: Address
  , price :: BigNumber
  , buyerFee :: BigNumber
  | r1
  } ->
  Web3
    { buyer :: Address
    , tokenId :: UIntN S256
    , contractAddress :: Address
    , owner :: Address
    , price :: BigNumber
    , buyerFee :: BigNumber
    , purchaseTxHash :: HexString
    | r1
    }
safeBuy tenv pd = do
  let
    { v2Marketplace: { deployAddress: marketContract }
    } = tenv

    { tokenId, contractAddress, price, buyerFee, buyer, owner } = pd
  txHash <-
    SuperRareMarketAuctionV2.safeBuy
      ( defaultTxOpts buyer
          # _to
          ?~ marketContract
          # _value
          ?~ fromMinorUnit (price + buyerFee)
      )
      { _tokenId: tokenId, _originContract: contractAddress, _amount: uInt256FromBigNumber price }
  awaitTxSuccessWeb3 txHash
  pure $ Record.insert (SProxy :: _ "purchaseTxHash") txHash pd

-----------------------------------------------------------------------------
-- | buy
-----------------------------------------------------------------------------
buy ::
  forall r r1.
  Lacks "purchaseTxHash" r1 =>
  TestEnv r ->
  { buyer :: Address
  , tokenId :: UIntN S256
  , contractAddress :: Address
  , owner :: Address
  , price :: BigNumber
  , buyerFee :: BigNumber
  | r1
  } ->
  Web3
    { buyer :: Address
    , tokenId :: UIntN S256
    , contractAddress :: Address
    , owner :: Address
    , price :: BigNumber
    , buyerFee :: BigNumber
    , purchaseTxHash :: HexString
    | r1
    }
buy tenv pd = do
  let
    { v2Marketplace: { deployAddress: marketContract }
    } = tenv

    { tokenId, contractAddress, price, buyerFee, buyer, owner } = pd
  txHash <-
    SuperRareMarketAuctionV2.buy
      ( defaultTxOpts buyer
          # _to
          ?~ marketContract
          # _value
          ?~ fromMinorUnit (price + buyerFee)
      )
      { _tokenId: tokenId, _originContract: contractAddress }
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
  , contractAddress :: Address
  , owner :: Address
  , price :: BigNumber
  , buyerFee :: BigNumber
  | r1
  } ->
  Web3 HexString
expensiveWalletBid tenv pd = do
  let
    { v2Marketplace: { deployAddress: marketContract }
    , testExpensiveWallet: { deployAddress: walletContract }
    , primaryAccount
    } = tenv

    { tokenId, contractAddress, price, buyerFee, buyer, owner } = pd
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
        , _originContract: contractAddress
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
    , v2Marketplace: { deployAddress: marketContract }
    , primaryAccount
    } = tenv

    { claimer } = pd

    superHighLimit = unsafePartial fromJust $ parseBigNumber decimal "97123880"
  asOwnerOfContract tenv claimer walletContract do
    txHash <-
      TestExpensiveWallet.claimMoney
        ( defaultTxOpts claimer
            # _to
            ?~ walletContract
            # _gas
            ?~ superHighLimit
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

-----------------------------------------------------------------------------
-- | payments
-----------------------------------------------------------------------------
payments ::
  forall r. TestEnv r -> Address -> Web3 (UIntN S256)
payments tenv dest =
  let
    { v2Marketplace: { deployAddress }
    , primaryAccount
    } = tenv
  in
    throwOnCallError
      $ SuperRareMarketAuctionV2.payments
          (defaultTxOpts primaryAccount # _to ?~ deployAddress)
          Latest
          { dest }

-----------------------------------------------------------------------------
-- | assertFailBid
-----------------------------------------------------------------------------
assertFailBid ::
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
assertFailBid tenv pd = do
  let
    { v2Marketplace: { deployAddress: marketContract }
    , v2SuperRare: { deployAddress: originContract }
    , testAssertFailOnPay: { deployAddress: assertfailOnPayAddr }
    , primaryAccount
    } = tenv

    { tokenId, price, buyerFee, buyer, owner } = pd
  txHash <-
    TestAssertFailOnPay.bid
      ( defaultTxOpts buyer
          # _to
          ?~ assertfailOnPayAddr
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
-- | requireFailBid
-----------------------------------------------------------------------------
requireFailBid ::
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
requireFailBid tenv pd = do
  let
    { v2Marketplace: { deployAddress: marketContract }
    , v2SuperRare: { deployAddress: originContract }
    , testRequireFailOnPay: { deployAddress: requirefailOnPayAddr }
    , primaryAccount
    } = tenv

    { tokenId, price, buyerFee, buyer, owner } = pd
  txHash <-
    TestRequireFailOnPay.bid
      ( defaultTxOpts buyer
          # _to
          ?~ requirefailOnPayAddr
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
-- | revertFailBid
-----------------------------------------------------------------------------
revertFailBid ::
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
revertFailBid tenv pd = do
  let
    { v2Marketplace: { deployAddress: marketContract }
    , v2SuperRare: { deployAddress: originContract }
    , testRevertOnPay: { deployAddress: revertfailOnPayAddr }
    , primaryAccount
    } = tenv

    { tokenId, price, buyerFee, buyer, owner } = pd
  -- TODO :: for now using asOwnerOfContract since it uses same ownership
  txHash <-
    TestRevertFailOnPay.bid
      ( defaultTxOpts buyer
          # _to
          ?~ revertfailOnPayAddr
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
-- | assertWithContext
-----------------------------------------------------------------------------
assertWithContext :: forall a b m. Show b => MonadAff m => MonadError Error m => b -> m a -> m a
assertWithContext ctx f = catchError f \e -> throwError $ error ("Context: " <> show ctx <> "\n" <> message e)
