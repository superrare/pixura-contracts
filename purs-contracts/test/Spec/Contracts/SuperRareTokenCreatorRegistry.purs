module Test.Spec.Contracts.SuperRareTokenCreatorRegistry where

import Prelude

import Chanterelle.Internal.Deploy (DeployReceipt)
import Chanterelle.Internal.Types (NoArgs)
import Chanterelle.Test (buildTestConfig)
import Contracts.SuperRareTokenCreatorRegistry as SuperRareTokenCreatorRegistry
import Data.Array (filter, length, replicate, zipWith)
import Data.Array.Partial (head)
import Data.Either (isLeft)
import Data.Lens ((?~))
import Data.Maybe (Maybe(..), fromJust)
import Data.Symbol (SProxy(..))
import Data.Traversable (for)
import Deploy.Contracts.SuperRareTokenCreatorRegistry as SuperRareTokenCreatorRegistry
import Deploy.Contracts.SuperRareV2 as SuperRareV2
import Deploy.Utils (throwOnCallError)
import Effect.Aff (Aff, error, throwError, try)
import Network.Ethereum.Core.HexString (nullWord, takeHex)
import Network.Ethereum.Web3 (Address, ChainCursor(..), Provider, UIntN, Web3, _to, embed, mkAddress, unUIntN)
import Network.Ethereum.Web3.Solidity.Sizes (S256)
import Partial.Unsafe (unsafePartial)
import Record as Record
import Test.Spec (SpecT, beforeAll, describe, it, itOnly)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.Contracts.SupeRare as SupeRare
import Test.Spec.Contracts.SuperRareLegacy as SuperRareLegacySpec
import Test.Spec.Contracts.SuperRareLegacy.Actions as SuperRareLegacy
import Test.Spec.Contracts.SuperRareV2 as SuperRareV2Spec
import Test.Spec.Contracts.Utils (createTokensWithFunction, defaultTxOpts, intToUInt256, uInt256FromBigNumber, web3Test)

spec :: SpecT Aff Unit Aff Unit
spec =
  beforeAll init do
    describe "SuperRareTokenCreatorRegistry"
      $ it "should get the creator for a token using the IERC721Creator loaded initially"
          getCreatorUsingIERC721CreatorContract
      $ itOnly "should override creator by setting creator for token"
          overrideCreatorBySettingCreator
  where
  getCreatorUsingIERC721CreatorContract tenv@{ provider } =
    web3Test provider do
      let
        { v2SuperRare: { deployAddress: v2SuperRare } } = tenv
      tokenDetails <- createTokensWithFunction tenv 1 (SuperRareV2Spec.addNewToken tenv)
      void
        $ for tokenDetails \{ tokenId } -> do
            expectedCreator <- SuperRareV2Spec.tokenCreator tenv tokenId
            creator <- tokenCreator tenv v2SuperRare tokenId
            creator `shouldEqual` expectedCreator
  overrideCreatorBySettingCreator tenv@{ provider } =
    web3Test provider do
      let
        { v2SuperRare: { deployAddress: v2SuperRare } } = tenv
      tokenDetails <- createTokensWithFunction tenv 1 (SuperRareV2Spec.addNewToken tenv)
      void
        $ for tokenDetails \{ tokenId } -> do
            expectedCreator <- SuperRareV2Spec.tokenCreator tenv tokenId
            creator <- tokenCreator tenv v2SuperRare tokenId
            creator `shouldEqual` expectedCreator

-----------------------------------------------------------------------------
-- | Init
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

init :: Aff (TestEnv ())
init = do
  tenv@{ provider, v2SuperRare: { deployAddress: v2SuperRare }, accounts, primaryAccount } <- initSupeRareV2
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

setTokenCreator :: forall r. TestEnv r -> Address -> UIntN S256 -> UIntN S8 -> Web3 Address
setTokenCreator { srTokenCreatorRegistry: { deployAddress }, primaryAccount } _contractAddress _tokenId = do
  txHash <- SuperRareTokenCreatorRegistry.setTokenCreator
        (defaultTxOpts primaryAccount # _to ?~ deployAddress)
        Latest
        { _contractAddress, _tokenId,  }
  awaitTxSuccessWeb3 txHash

