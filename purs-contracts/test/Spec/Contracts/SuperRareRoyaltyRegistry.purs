module Test.Spec.Contracts.SuperRareRoyaltyRegistry where

import Prelude
import Chanterelle.Internal.Deploy (DeployReceipt)
import Chanterelle.Internal.Types (NoArgs)
import Chanterelle.Test (buildTestConfig)
import Contracts.SuperRareRoyaltyRegistry (getERC721TokenRoyaltyPercentage, setPercentageForSetERC721ContractRoyalty, setPercentageForSetERC721CreatorRoyalty, setPercentageForSetERC721TokenRoyalty, tokenCreator) as SuperRareRoyaltyRegistry
import Control.Monad.Error.Class (class MonadThrow)
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Data.Traversable (for)
import Deploy.Contracts.SuperRareRoyaltyRegistry (SuperRareRoyaltyRegistry, deployScript) as SuperRareRoyaltyRegistry
import Deploy.Contracts.SuperRareTokenCreatorRegistry (SuperRareTokenCreatorRegistry) as SuperRareTokenCreatorRegistry
import Deploy.Contracts.SuperRareV2 as SuperRareV2
import Deploy.Utils (awaitTxSuccessWeb3, throwOnCallError)
import Effect.Aff (Aff, Error)
import Effect.Aff.Class (class MonadAff)
import Network.Ethereum.Web3 (Address, ChainCursor(..), Provider, UIntN, Web3, _to, embed, uIntNFromBigNumber)
import Network.Ethereum.Web3.Solidity.Sizes (S256, S8, s8)
import Partial.Unsafe (unsafePartial)
import Record as Record
import Test.Spec (SpecT, beforeAll, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Contracts.SuperRareTokenCreatorRegistry (init) as SuperRareTokenCreatorRegistry
import Test.Spec.Contracts.SuperRareV2 as SuperRareV2Spec
import Test.Spec.Contracts.Utils (createTokensWithFunction, defaultTxOpts, web3Test)

-----------------------------------------------------------------------------
--- | spec
-----------------------------------------------------------------------------
spec :: SpecT Aff Unit Aff Unit
spec =
  beforeAll init do
    describe "SuperRareRoyaltyRegistry" do
      it "should default to 0 percent royalty for token"
        defaultRoyaltyShouldBeZero
      it "should use token contract royalty when set"
        shouldUseTokenContractRoyalty
      it "should use creator royalty when set"
        shouldUseCreatorRoyalty
      it "should use token royalty when set"
        shouldUseTokenRoyalty
      it "should use the the following priority: token royalty > creator royalty > contract royalty"
        shouldUseRoyaltyPriority

-----------------------------------------------------------------------------
--- | defaultRoyaltyShouldBeZero
-----------------------------------------------------------------------------
defaultRoyaltyShouldBeZero ::
  forall m r.
  MonadAff m =>
  MonadThrow Error m =>
  TestEnv r ->
  m Unit
defaultRoyaltyShouldBeZero tenv@{ provider } =
  web3Test provider do
    tokenDetails <- createTokensWithFunction tenv 1 (SuperRareV2Spec.addNewToken tenv)
    void
      $ for tokenDetails \{ tokenId, contractAddress } -> do
          let
            expectedPercentage = unsafePartial fromJust $ uIntNFromBigNumber (s8) (embed 0)
          percentage <- getERC721TokenRoyaltyPercentage tenv contractAddress tokenId
          percentage `shouldEqual` expectedPercentage

-----------------------------------------------------------------------------
--- | shouldUseTokenContractRoyalty
-----------------------------------------------------------------------------
shouldUseTokenContractRoyalty ::
  forall m r.
  MonadAff m =>
  MonadThrow Error m =>
  TestEnv r ->
  m Unit
shouldUseTokenContractRoyalty tenv@{ provider } =
  web3Test provider do
    tokenDetails <- createTokensWithFunction tenv 1 (SuperRareV2Spec.addNewToken tenv)
    void
      $ for tokenDetails \{ tokenId, contractAddress } -> do
          let
            expectedPercentage = unsafePartial fromJust $ uIntNFromBigNumber (s8) (embed 10)
          setPercentageForSetERC721ContractRoyalty tenv contractAddress expectedPercentage
          percentage <- getERC721TokenRoyaltyPercentage tenv contractAddress tokenId
          percentage `shouldEqual` expectedPercentage

-----------------------------------------------------------------------------
--- | shouldUseCreatorRoyalty
-----------------------------------------------------------------------------
shouldUseCreatorRoyalty ::
  forall m r.
  MonadAff m =>
  MonadThrow Error m =>
  TestEnv r ->
  m Unit
shouldUseCreatorRoyalty tenv@{ provider } =
  web3Test provider do
    tokenDetails <- createTokensWithFunction tenv 1 (SuperRareV2Spec.addNewToken tenv)
    void
      $ for tokenDetails \{ tokenId, contractAddress } -> do
          let
            expectedPercentage = unsafePartial fromJust $ uIntNFromBigNumber (s8) (embed 10)
          creator <- tokenCreator tenv contractAddress tokenId
          setPercentageForSetERC721CreatorRoyalty tenv creator expectedPercentage
          percentage <- getERC721TokenRoyaltyPercentage tenv contractAddress tokenId
          percentage `shouldEqual` expectedPercentage

-----------------------------------------------------------------------------
--- | shouldUseTokenRoyalty
-----------------------------------------------------------------------------
shouldUseTokenRoyalty ::
  forall m r.
  MonadAff m =>
  MonadThrow Error m =>
  TestEnv r ->
  m Unit
shouldUseTokenRoyalty tenv@{ provider } =
  web3Test provider do
    tokenDetails <- createTokensWithFunction tenv 1 (SuperRareV2Spec.addNewToken tenv)
    void
      $ for tokenDetails \{ tokenId, contractAddress } -> do
          let
            expectedPercentage = unsafePartial fromJust $ uIntNFromBigNumber (s8) (embed 10)
          setPercentageForSetERC721TokenRoyalty tenv contractAddress tokenId expectedPercentage
          percentage <- getERC721TokenRoyaltyPercentage tenv contractAddress tokenId
          percentage `shouldEqual` expectedPercentage

-----------------------------------------------------------------------------
--- | shouldUseRoyaltyPriority
-----------------------------------------------------------------------------
shouldUseRoyaltyPriority ::
  forall m r.
  MonadAff m =>
  MonadThrow Error m =>
  TestEnv r ->
  m Unit
shouldUseRoyaltyPriority tenv@{ provider } =
  web3Test provider do
    tokenDetails <- createTokensWithFunction tenv 1 (SuperRareV2Spec.addNewToken tenv)
    void
      $ for tokenDetails \{ tokenId, contractAddress } -> do
          let
            expectedPercentageContract = unsafePartial fromJust $ uIntNFromBigNumber (s8) (embed 3)

            expectedPercentageCreator = unsafePartial fromJust $ uIntNFromBigNumber (s8) (embed 5)

            expectedPercentageToken = unsafePartial fromJust $ uIntNFromBigNumber (s8) (embed 10)
          setPercentageForSetERC721ContractRoyalty tenv contractAddress expectedPercentageContract
          creator <- tokenCreator tenv contractAddress tokenId
          setPercentageForSetERC721CreatorRoyalty tenv creator expectedPercentageCreator
          percentageCreator <- getERC721TokenRoyaltyPercentage tenv contractAddress tokenId
          percentageCreator `shouldEqual` expectedPercentageCreator
          setPercentageForSetERC721TokenRoyalty tenv contractAddress tokenId expectedPercentageToken
          percentageToken <- getERC721TokenRoyaltyPercentage tenv contractAddress tokenId
          percentageToken `shouldEqual` expectedPercentageToken

-----------------------------------------------------------------------------
--- | Init
-----------------------------------------------------------------------------
type TestEnv r
  = { supeRare :: DeployReceipt NoArgs
    , provider :: Provider
    , accounts :: Array Address
    , primaryAccount :: Address
    , v2SuperRare :: DeployReceipt SuperRareV2.SuperRareV2
    , srTokenCreatorRegistry :: DeployReceipt SuperRareTokenCreatorRegistry.SuperRareTokenCreatorRegistry
    , srRoyaltyRegistry :: DeployReceipt SuperRareRoyaltyRegistry.SuperRareRoyaltyRegistry
    | r
    }

init :: Aff (TestEnv ())
init = do
  tenv@{ provider
  , srTokenCreatorRegistry: { deployAddress: srTokenCreatorRegistry }
  , accounts
  , primaryAccount
  } <-
    SuperRareTokenCreatorRegistry.init
  { superRareRoyaltyRegistry } <-
    buildTestConfig "http://localhost:8545" 60
      $ SuperRareRoyaltyRegistry.deployScript
          { _iERC721TokenCreator: srTokenCreatorRegistry }
  pure
    $ Record.merge { srRoyaltyRegistry: superRareRoyaltyRegistry } tenv

-----------------------------------------------------------------------------
--- | Contract Functions
-----------------------------------------------------------------------------
getERC721TokenRoyaltyPercentage :: forall r. TestEnv r -> Address -> UIntN S256 -> Web3 (UIntN S8)
getERC721TokenRoyaltyPercentage { srRoyaltyRegistry: { deployAddress }, primaryAccount } _contractAddress _tokenId =
  throwOnCallError
    $ SuperRareRoyaltyRegistry.getERC721TokenRoyaltyPercentage
        (defaultTxOpts primaryAccount # _to ?~ deployAddress)
        Latest
        { _contractAddress, _tokenId }

setPercentageForSetERC721ContractRoyalty :: forall r. TestEnv r -> Address -> UIntN S8 -> Web3 Unit
setPercentageForSetERC721ContractRoyalty { srRoyaltyRegistry: { deployAddress }, primaryAccount } _contractAddress _percentage = do
  txHash <-
    SuperRareRoyaltyRegistry.setPercentageForSetERC721ContractRoyalty
      (defaultTxOpts primaryAccount # _to ?~ deployAddress)
      { _contractAddress, _percentage }
  awaitTxSuccessWeb3 txHash

tokenCreator :: forall r. TestEnv r -> Address -> UIntN S256 -> Web3 Address
tokenCreator { srRoyaltyRegistry: { deployAddress }, primaryAccount } _contractAddress _tokenId =
  throwOnCallError
    $ SuperRareRoyaltyRegistry.tokenCreator
        (defaultTxOpts primaryAccount # _to ?~ deployAddress)
        Latest
        { _contractAddress, _tokenId }

setPercentageForSetERC721CreatorRoyalty :: forall r. TestEnv r -> Address -> UIntN S8 -> Web3 Unit
setPercentageForSetERC721CreatorRoyalty { srRoyaltyRegistry: { deployAddress }, primaryAccount } _creatorAddress _percentage = do
  txHash <-
    SuperRareRoyaltyRegistry.setPercentageForSetERC721CreatorRoyalty
      (defaultTxOpts primaryAccount # _to ?~ deployAddress)
      { _creatorAddress, _percentage }
  awaitTxSuccessWeb3 txHash

setPercentageForSetERC721TokenRoyalty :: forall r. TestEnv r -> Address -> UIntN S256 -> UIntN S8 -> Web3 Unit
setPercentageForSetERC721TokenRoyalty { srRoyaltyRegistry: { deployAddress }, primaryAccount } _contractAddress _tokenId _percentage = do
  txHash <-
    SuperRareRoyaltyRegistry.setPercentageForSetERC721TokenRoyalty
      (defaultTxOpts primaryAccount # _to ?~ deployAddress)
      { _contractAddress, _tokenId, _percentage }
  awaitTxSuccessWeb3 txHash
