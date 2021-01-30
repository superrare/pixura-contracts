module Test.Spec.Contracts.SuperRareTokenCreatorRegistry where

import Prelude
import Chanterelle.Internal.Deploy (DeployReceipt)
import Chanterelle.Internal.Types (NoArgs)
import Chanterelle.Test (buildTestConfig)
import Contracts.SuperRareTokenCreatorRegistry (setTokenCreator, tokenCreator) as SuperRareTokenCreatorRegistry
import Control.Monad.Error.Class (class MonadThrow)
import Data.Array (filter)
import Data.Array.Partial (head)
import Data.Lens ((?~))
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Deploy.Contracts.SuperRareTokenCreatorRegistry (SuperRareTokenCreatorRegistry, deployScript) as SuperRareTokenCreatorRegistry
import Deploy.Contracts.SuperRareV2 as SuperRareV2
import Deploy.Utils (awaitTxSuccessWeb3, throwOnCallError)
import Effect.Aff (Aff, Error)
import Effect.Aff.Class (class MonadAff)
import Network.Ethereum.Web3 (Address, ChainCursor(..), Provider, UIntN, Web3, _to)
import Network.Ethereum.Web3.Solidity.Sizes (S256)
import Partial.Unsafe (unsafePartial)
import Record as Record
import Test.Spec (SpecT, beforeAll, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)
import Test.Spec.Contracts.SuperRareV2 as SuperRareV2Spec
import Test.Spec.Contracts.Utils (createTokensWithFunction, defaultTxOpts, web3Test)

-----------------------------------------------------------------------------
--- | spec
-----------------------------------------------------------------------------
spec :: SpecT Aff Unit Aff Unit
spec =
  beforeAll (init Nothing) do
    describe "SuperRareTokenCreatorRegistry" do
      it "should get the creator for a token using the IERC721Creator loaded initially"
        getCreatorUsingIERC721CreatorContract
      it "should override creator by setting creator for token"
        overrideCreatorBySettingCreator

-----------------------------------------------------------------------------
--- | getCreatorUsingIERC721CreatorContract
-----------------------------------------------------------------------------
getCreatorUsingIERC721CreatorContract ::
  forall m r.
  MonadAff m =>
  MonadThrow Error m =>
  TestEnv r ->
  m Unit
getCreatorUsingIERC721CreatorContract tenv@{ provider } =
  web3Test provider do
    tokenDetails <- createTokensWithFunction tenv 1 (SuperRareV2Spec.addNewToken tenv)
    void
      $ for tokenDetails \{ tokenId, contractAddress } -> do
          expectedCreator <- SuperRareV2Spec.tokenCreator tenv tokenId
          creator <- tokenCreator tenv contractAddress tokenId
          creator `shouldEqual` expectedCreator

-----------------------------------------------------------------------------
--- | overrideCreatorBySettingCreator
-----------------------------------------------------------------------------
overrideCreatorBySettingCreator ::
  forall m r.
  MonadAff m =>
  MonadThrow Error m =>
  TestEnv r ->
  m Unit
overrideCreatorBySettingCreator tenv@{ provider } =
  web3Test provider do
    let
      { v2SuperRare: { deployAddress: v2SuperRare }, accounts } = tenv
    tokenDetails <- createTokensWithFunction tenv 1 (SuperRareV2Spec.addNewToken tenv)
    void
      $ for tokenDetails \{ tokenId, contractAddress } -> do
          initialCreator <- tokenCreator tenv contractAddress tokenId
          let
            newCreator = unsafePartial head $ filter ((/=) initialCreator) accounts
          setTokenCreator tenv contractAddress tokenId newCreator
          onChainCreator <- tokenCreator tenv contractAddress tokenId
          initialCreator `shouldNotEqual` onChainCreator
          newCreator `shouldEqual` onChainCreator

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
    | r
    }

init :: Maybe (SuperRareV2Spec.TestEnv ()) -> Aff (TestEnv ())
init msrv2Tenv = do
  tenv@{ provider, v2SuperRare: { deployAddress: v2SuperRare }, accounts, primaryAccount } <- case msrv2Tenv of
    Nothing -> initSupeRareV2
    Just srt -> pure srt
  { superRareTokenCreatorRegistry } <-
    buildTestConfig "http://localhost:8545" 60
      $ SuperRareTokenCreatorRegistry.deployScript
          { _iERC721Creators: [ v2SuperRare ] }
  pure
    $ Record.merge
        { srTokenCreatorRegistry: superRareTokenCreatorRegistry
        }
        tenv
  where
  initSupeRareV2 = do
    tenv@{ accounts, provider } <- SuperRareV2Spec.init
    web3Test provider $ whitelistAddresses tenv
    pure tenv

  whitelistAddresses tenv@{ accounts } = void $ for accounts (SuperRareV2Spec.whitelistAddress tenv)

-----------------------------------------------------------------------------
--- | Contract Functions
-----------------------------------------------------------------------------
tokenCreator :: forall r. TestEnv r -> Address -> UIntN S256 -> Web3 Address
tokenCreator { srTokenCreatorRegistry: { deployAddress }, primaryAccount } _contractAddress _tokenId =
  throwOnCallError
    $ SuperRareTokenCreatorRegistry.tokenCreator
        (defaultTxOpts primaryAccount # _to ?~ deployAddress)
        Latest
        { _contractAddress, _tokenId }

setTokenCreator :: forall r. TestEnv r -> Address -> UIntN S256 -> Address -> Web3 Unit
setTokenCreator { srTokenCreatorRegistry: { deployAddress }, primaryAccount } _contractAddress _tokenId _creator = do
  txHash <-
    SuperRareTokenCreatorRegistry.setTokenCreator
      (defaultTxOpts primaryAccount # _to ?~ deployAddress)
      { _contractAddress, _tokenId, _creator }
  awaitTxSuccessWeb3 txHash
