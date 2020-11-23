module Test.Spec.Contracts.MarketplaceSettings where

import Prelude
import Chanterelle.Internal.Deploy (DeployReceipt)
import Chanterelle.Internal.Types (NoArgs)
import Chanterelle.Test (buildTestConfig)
import Contracts.Marketplace.MarketplaceSettings as MarketplaceSettings
import Control.Monad.Error.Class (class MonadThrow)
import Data.Array (drop, filter, take)
import Data.Array.Partial (head)
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Data.Traversable (for)
import Deploy.Contracts.MarketplaceSettings (deployScript) as MarketplaceSettings
import Deploy.Contracts.SuperRareV2 as SuperRareV2
import Deploy.Utils (awaitTxSuccessWeb3, throwOnCallError)
import Effect.Aff (Aff, Error)
import Effect.Aff.Class (class MonadAff)
import Network.Ethereum.Web3 (Address, ChainCursor(..), Provider, UIntN, Web3, _to, embed, uIntNFromBigNumber)
import Network.Ethereum.Web3.Solidity.Sizes (S256, S8, s256, s8)
import Partial.Unsafe (unsafePartial)
import Record as Record
import Test.Spec (SpecT, beforeAll, describe, describeOnly, it, itOnly)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)
import Test.Spec.Contracts.SuperRareV2 as SuperRareV2Spec
import Test.Spec.Contracts.Utils (createTokensWithFunction, defaultTxOpts, web3Test)

-----------------------------------------------------------------------------
--- | spec
-----------------------------------------------------------------------------
spec :: SpecT Aff Unit Aff Unit
spec =
  beforeAll init do
    describe "MarketplaceSettings" do
      it "should get and set marketplace percentage"
        shouldGetAndSetMarketplacePercentage
      it "should get and set max wei marketplace"
        shouldGetAndSetMaxValue
      it "should get and set min wei marketplace"
        shouldGetAndSetMinValue

-----------------------------------------------------------------------------
--- | shouldGetAndSetMarketplacePercentage
-----------------------------------------------------------------------------
shouldGetAndSetMarketplacePercentage ::
  forall m r.
  MonadAff m =>
  MonadThrow Error m =>
  TestEnv r ->
  m Unit
shouldGetAndSetMarketplacePercentage tenv@{ provider } =
  web3Test provider do
    let
      expectedPercentageInitial = unsafePartial fromJust $ uIntNFromBigNumber (s8) (embed 3)

      expectedPercentageNew = unsafePartial fromJust $ uIntNFromBigNumber (s8) (embed 10)
    percentage <- getMarketplaceFeePercentage tenv
    percentage `shouldEqual` expectedPercentageInitial
    setMarketplaceFeePercentage tenv expectedPercentageNew
    percentageNew <- getMarketplaceFeePercentage tenv
    percentageNew `shouldEqual` expectedPercentageNew

-----------------------------------------------------------------------------
--- | shouldGetAndSetMaxValue
-----------------------------------------------------------------------------
shouldGetAndSetMaxValue ::
  forall m r.
  MonadAff m =>
  MonadThrow Error m =>
  TestEnv r ->
  m Unit
shouldGetAndSetMaxValue tenv@{ provider } =
  web3Test provider do
    let
      expectedMaxValue = unsafePartial fromJust $ uIntNFromBigNumber (s256) (embed 1000000)
    setMarketplaceMaxValue tenv expectedMaxValue
    maxValue <- getMarketplaceMaxValue tenv
    maxValue `shouldEqual` expectedMaxValue

-----------------------------------------------------------------------------
--- | shouldGetAndSetMinValue
-----------------------------------------------------------------------------
shouldGetAndSetMinValue ::
  forall m r.
  MonadAff m =>
  MonadThrow Error m =>
  TestEnv r ->
  m Unit
shouldGetAndSetMinValue tenv@{ provider } =
  web3Test provider do
    let
      expectedMinValue = unsafePartial fromJust $ uIntNFromBigNumber (s256) (embed 10)
    setMarketplaceMinValue tenv expectedMinValue
    minValue <- getMarketplaceMinValue tenv
    minValue `shouldEqual` expectedMinValue

-----------------------------------------------------------------------------
--- | Init
-----------------------------------------------------------------------------
type TestEnv r
  = { provider :: Provider
    , accounts :: Array Address
    , primaryAccount :: Address
    , marketplaceSettings :: DeployReceipt NoArgs
    | r
    }

init :: Aff (TestEnv ())
init = do
  { marketplaceSettings, provider, accounts } <-
    buildTestConfig "http://localhost:8545" 60 MarketplaceSettings.deployScript
  pure { provider, marketplaceSettings, accounts: take 4 $ drop 2 accounts, primaryAccount: unsafePartial head accounts }

-----------------------------------------------------------------------------
--- | Contract Functions
-----------------------------------------------------------------------------
getMarketplaceFeePercentage :: forall r. TestEnv r -> Web3 (UIntN S8)
getMarketplaceFeePercentage { marketplaceSettings: { deployAddress }, primaryAccount } =
  throwOnCallError
    $ MarketplaceSettings.getMarketplaceFeePercentage
        (defaultTxOpts primaryAccount # _to ?~ deployAddress)
        Latest

setMarketplaceFeePercentage :: forall r. TestEnv r -> UIntN S8 -> Web3 Unit
setMarketplaceFeePercentage { marketplaceSettings: { deployAddress }, primaryAccount } _percentage = do
  txHash <-
    MarketplaceSettings.setMarketplaceFeePercentage
      (defaultTxOpts primaryAccount # _to ?~ deployAddress)
      { _percentage }
  awaitTxSuccessWeb3 txHash

getMarketplaceMaxValue :: forall r. TestEnv r -> Web3 (UIntN S256)
getMarketplaceMaxValue { marketplaceSettings: { deployAddress }, primaryAccount } =
  throwOnCallError
    $ MarketplaceSettings.getMarketplaceMaxValue
        (defaultTxOpts primaryAccount # _to ?~ deployAddress)
        Latest

setMarketplaceMaxValue :: forall r. TestEnv r -> UIntN S256 -> Web3 Unit
setMarketplaceMaxValue { marketplaceSettings: { deployAddress }, primaryAccount } _maxValue = do
  txHash <-
    MarketplaceSettings.setMarketplaceMaxValue
      (defaultTxOpts primaryAccount # _to ?~ deployAddress)
      { _maxValue }
  awaitTxSuccessWeb3 txHash

getMarketplaceMinValue :: forall r. TestEnv r -> Web3 (UIntN S256)
getMarketplaceMinValue { marketplaceSettings: { deployAddress }, primaryAccount } =
  throwOnCallError
    $ MarketplaceSettings.getMarketplaceMinValue
        (defaultTxOpts primaryAccount # _to ?~ deployAddress)
        Latest

setMarketplaceMinValue :: forall r. TestEnv r -> UIntN S256 -> Web3 Unit
setMarketplaceMinValue { marketplaceSettings: { deployAddress }, primaryAccount } _minValue = do
  txHash <-
    MarketplaceSettings.setMarketplaceMinValue
      (defaultTxOpts primaryAccount # _to ?~ deployAddress)
      { _minValue }
  awaitTxSuccessWeb3 txHash

giveContractMarkTokenRole :: forall r. TestEnv r -> Address -> Web3 Unit
giveContractMarkTokenRole { marketplaceSettings: { deployAddress }, primaryAccount } granteeAddress = do
  tokenMarkRole <-
    throwOnCallError
      $ MarketplaceSettings.tOKEN_MARK_ROLE
          (defaultTxOpts primaryAccount # _to ?~ deployAddress)
          Latest
  txHash <-
    MarketplaceSettings.grantRole
      ( defaultTxOpts primaryAccount
          # _to
          ?~ deployAddress
      )
      { account: granteeAddress, role: tokenMarkRole }
  awaitTxSuccessWeb3 txHash
