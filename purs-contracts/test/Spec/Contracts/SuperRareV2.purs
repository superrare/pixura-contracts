module Test.Spec.Contracts.SuperRareV2 where

import Prelude
import Chanterelle.Internal.Deploy (DeployReceipt)
import Chanterelle.Internal.Types (NoArgs)
import Chanterelle.Test (buildTestConfig)
import Contracts.SuperRareV2 (addNewToken, addToWhitelist, isApprovedForAll, isWhitelisted, ownerOf, setApprovalForAll, tokenByIndex, tokenCreator, tokenURI, totalSupply, transferFrom) as SuperRareV2
import Data.Array (filter, length, replicate)
import Data.Array.Partial (head)
import Data.Lens ((?~))
import Data.Symbol (SProxy(..))
import Data.Traversable (for)
import Deploy.Contracts.SuperRareV2 (SuperRareV2, deployScript) as SuperRareV2
import Deploy.Utils (awaitTxSuccessWeb3)
import Effect.Aff (Aff)
import Network.Ethereum.Core.BigNumber (unsafeToInt)
import Network.Ethereum.Web3 (Address, ChainCursor(..), Provider, UIntN, Web3, _to, unUIntN)
import Network.Ethereum.Web3.Solidity.Sizes (S256)
import Partial.Unsafe (unsafePartial)
import Record as Record
import Test.Spec (SpecT, beforeAll, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Contracts.SupeRare as SupeRare
import Test.Spec.Contracts.Utils (createTokensWithFunction, defaultTxOpts, intToUInt256, throwOnCallError, web3Test)

spec :: SpecT Aff Unit Aff Unit
spec =
  beforeAll init do
    describe "SuperRareV2" do
      it "can whitelist accounts" \tenv@{ provider, accounts } -> do
        web3Test provider do
          void $ for accounts $ whitelistAddress tenv
          allIsWhitelistedRes <- for accounts $ isWhitelisted tenv
          allIsWhitelistedRes `shouldEqual` replicate (length accounts) true
      it "can mint tokens" \tenv@{ provider, accounts } -> do
        web3Test provider do
          lastId <- (unsafeToInt <<< unUIntN) <$> SupeRare.totalSupply tenv
          tokenDetails <- createTokensWithFunction tenv 1 (addNewToken tenv)
          let
            tokenIds = tokenDetails <#> \{ tokenId } -> tokenId
          owners <- for tokenIds (ownerOf tenv)
          owners `shouldEqual` (tokenDetails <#> \{ owner } -> owner)
          uris <- for tokenIds (tokenURI tenv)
          uris `shouldEqual` (tokenDetails <#> \{ uri } -> uri)
      it "can transfer tokens" \tenv@{ provider, accounts } -> do
        web3Test provider do
          tokenDetails <- createTokensWithFunction tenv 1 (addNewToken tenv)
          updatedTokenDetails <-
            for tokenDetails \td@{ tokenId, owner } -> do
              let
                newOwner = unsafePartial head $ filter ((/=) owner) accounts
              transferFrom tenv owner owner newOwner tokenId
              pure $ td { owner = newOwner }
          owners <- for (updatedTokenDetails <#> \{ tokenId } -> tokenId) (ownerOf tenv)
          owners `shouldEqual` (updatedTokenDetails <#> \{ owner } -> owner)
      it "can approve others to manage tokens" \tenv@{ provider, accounts } -> do
        web3Test provider do
          tokenDetails <- createTokensWithFunction tenv 1 (addNewToken tenv)
          updatedTokenDetails <-
            for tokenDetails \td@{ owner } -> do
              let
                approvedOperator = unsafePartial head $ filter ((/=) owner) accounts
              setApprovalForAll tenv owner approvedOperator true
              pure $ Record.insert (SProxy :: _ "approvedOperator") approvedOperator td
          allIsApprovedForAlls <-
            for updatedTokenDetails \{ tokenId, approvedOperator, owner } ->
              isApprovedForAll tenv owner approvedOperator
          allIsApprovedForAlls `shouldEqual` replicate (length updatedTokenDetails) true
          void
            $ for updatedTokenDetails \{ tokenId, approvedOperator, owner } ->
                transferFrom tenv approvedOperator owner approvedOperator tokenId

-----------------------------------------------------------------------------
-- | TestEnv
-----------------------------------------------------------------------------
type TestEnv r
  = { supeRare :: DeployReceipt NoArgs
    , provider :: Provider
    , accounts :: Array Address
    , primaryAccount :: Address
    , v2SuperRare :: DeployReceipt SuperRareV2.SuperRareV2
    | r
    }

init :: Aff (TestEnv ())
init = do
  tenv <- initSupeRareOld
  { provider, superRareV2, accounts } <-
    buildTestConfig "http://localhost:8545" 60
      $ SuperRareV2.deployScript
          { _name: "SuperRare"
          , _symbol: "SUPR"
          , _oldSuperRare: tenv.supeRare.deployAddress
          }
  pure $ Record.insert (SProxy :: _ "v2SuperRare") superRareV2 tenv
  where
  initSupeRareOld = do
    tenv@{ accounts, provider } <- SupeRare.init
    web3Test provider do
      whitelistAddresses tenv
      createOldSupeRareTokens tenv
    pure tenv

  whitelistAddresses tenv@{ accounts } = void $ for accounts (SupeRare.whitelistAddress tenv)

  createOldSupeRareTokens tenv = void $ createTokensWithFunction tenv 2 (SupeRare.addNewToken tenv)

-----------------------------------------------------------------------------
-- | Utils
-----------------------------------------------------------------------------
addNewToken :: forall r. TestEnv r -> Address -> String -> Web3 { tokenId :: UIntN S256, contractAddress :: Address }
addNewToken tenv@{ v2SuperRare: { deployAddress }, primaryAccount } from _uri = do
  SuperRareV2.addNewToken (defaultTxOpts from # _to ?~ deployAddress)
    { _uri }
    >>= awaitTxSuccessWeb3
  supply <- (unsafeToInt <<< unUIntN) <$> totalSupply tenv
  tid <- tokenByIndex tenv (intToUInt256 (supply - 1))
  pure { tokenId: tid, contractAddress: deployAddress }

transferFrom :: forall r. TestEnv r -> Address -> Address -> Address -> UIntN S256 -> Web3 Unit
transferFrom tenv@{ v2SuperRare: { deployAddress }, primaryAccount } signer from to tokenId = do
  SuperRareV2.transferFrom (defaultTxOpts from # _to ?~ deployAddress)
    { from, to, tokenId }
    >>= awaitTxSuccessWeb3

setApprovalForAll :: forall r. TestEnv r -> Address -> Address -> Boolean -> Web3 Unit
setApprovalForAll tenv from to approved =
  let
    { v2SuperRare: { deployAddress } } = tenv
  in
    SuperRareV2.setApprovalForAll (defaultTxOpts from # _to ?~ deployAddress)
      { to, approved }
      >>= awaitTxSuccessWeb3

whitelistAddress :: forall r. TestEnv r -> Address -> Web3 Unit
whitelistAddress { v2SuperRare: { deployAddress }, primaryAccount } _newAddress =
  SuperRareV2.addToWhitelist
    (defaultTxOpts primaryAccount # _to ?~ deployAddress)
    { _newAddress }
    >>= awaitTxSuccessWeb3

tokenURI :: forall r. TestEnv r -> UIntN S256 -> Web3 String
tokenURI { v2SuperRare: { deployAddress }, primaryAccount } tokenId =
  throwOnCallError
    $ SuperRareV2.tokenURI
        (defaultTxOpts primaryAccount # _to ?~ deployAddress)
        Latest
        { tokenId }

isWhitelisted :: forall r. TestEnv r -> Address -> Web3 Boolean
isWhitelisted { v2SuperRare: { deployAddress }, primaryAccount } _address =
  throwOnCallError
    $ SuperRareV2.isWhitelisted
        (defaultTxOpts primaryAccount # _to ?~ deployAddress)
        Latest
        { _address }

isApprovedForAll :: forall r. TestEnv r -> Address -> Address -> Web3 Boolean
isApprovedForAll { v2SuperRare: { deployAddress }, primaryAccount } owner operator =
  throwOnCallError
    $ SuperRareV2.isApprovedForAll
        (defaultTxOpts primaryAccount # _to ?~ deployAddress)
        Latest
        { owner, operator }

ownerOf :: forall r. TestEnv r -> UIntN S256 -> Web3 Address
ownerOf { v2SuperRare: { deployAddress }, primaryAccount } tokenId =
  throwOnCallError
    $ SuperRareV2.ownerOf
        (defaultTxOpts primaryAccount # _to ?~ deployAddress)
        Latest
        { tokenId }

tokenCreator :: forall r. TestEnv r -> UIntN S256 -> Web3 Address
tokenCreator { v2SuperRare: { deployAddress }, primaryAccount } _tokenId =
  throwOnCallError
    $ SuperRareV2.tokenCreator
        (defaultTxOpts primaryAccount # _to ?~ deployAddress)
        Latest
        { _tokenId }

totalSupply :: forall r. TestEnv r -> Web3 (UIntN S256)
totalSupply { v2SuperRare: { deployAddress }, primaryAccount } =
  throwOnCallError
    $ SuperRareV2.totalSupply
        (defaultTxOpts primaryAccount # _to ?~ deployAddress)
        Latest

tokenByIndex :: forall r. TestEnv r -> UIntN S256 -> Web3 (UIntN S256)
tokenByIndex { v2SuperRare: { deployAddress }, primaryAccount } index =
  throwOnCallError
    $ SuperRareV2.tokenByIndex
        (defaultTxOpts primaryAccount # _to ?~ deployAddress)
        Latest
        { index }
