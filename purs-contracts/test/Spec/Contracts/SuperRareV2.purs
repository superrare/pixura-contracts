module Test.Spec.Contracts.SuperRareV2 where

import Prelude
import Chanterelle.Internal.Deploy (DeployReceipt)
import Chanterelle.Internal.Types (NoArgs)
import Chanterelle.Test (buildTestConfig)
import Contracts.SuperRareV2 (addNewToken, tokenByIndex, addToWhitelist, isWhitelisted, ownerOf, tokenURI, totalSupply, transferFrom) as SuperRareV2
import Data.Array (elem, filter, length, replicate, take, zip, zipWith, (..))
import Data.Array.Partial (head, last)
import Data.Either (Either(..), fromRight)
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Data.Symbol (SProxy(..))
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..), fst)
import Deploy.Contracts.SuperRareV2 (SuperRareV2, deployScript) as SuperRareV2
import Deploy.Utils (awaitTxSuccessWeb3)
import Effect.Aff (Aff)
import Effect.Aff.AVar (put)
import Effect.Aff.Class (liftAff)
import Network.Ethereum.Core.BigNumber (decimal, embed, parseBigNumber, unsafeToInt)
import Network.Ethereum.Web3 (Address, ChainCursor(..), Provider, TransactionOptions, UIntN, Web3, _from, _gas, _gasPrice, _to, defaultTransactionOptions, uIntNFromBigNumber, unUIntN)
import Network.Ethereum.Web3.Solidity.Sizes (S256, s256)
import Network.Ethereum.Web3.Types (NoPay)
import Partial.Unsafe (unsafePartial)
import Record as Record
import Test.Spec (SpecT, beforeAll, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Contracts.SupeRare as SupeRare
import Test.Spec.Contracts.Utils (defaultTxOpts, intToUInt256, mkTokenUris, readOrFail, throwOnCallError, web3Test)

spec :: SpecT Aff Unit Aff Unit
spec =
  beforeAll init
    $ do
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

-- it "can transfer tokens" \tenv { provider } -> do
--   web3Test provider do
--     finalIndex <- (unsafeToInt <<< unUIntN) <$> totalSupply tenv
--     latestTokenId <- tokenByIndex tenv (intToUInt256 (finalIndex - 1))
--     let
--       transferV2Tokens = (take 2 v2Tokens)
--     ownerAndTokens <-
--       for transferV2Tokens \tokenId -> do
--         owner <-
--           map (unsafePartial fromRight $ SuperRareV2.ownerOf)
--             (defaultTxOpts primaryAccount # _to ?~ v2SuperRare.deployAddress)
--             Latest
--             { tokenId }
--         pure (Tuple owner tokenId)
--     let
--       transferToAddrs = filter (\acc -> not $ elem acc (map fst ownerAndTokens)) accounts
--       transferPayloads = zip ownerAndTokens transferToAddrs
--     void $ for transferPayloads
--       $ \(Tuple (Tuple from tokenId) to) -> do
--           SuperRareV2.transferFrom
--             (defaultTxOpts from # _to ?~ v2SuperRare.deployAddress)
--             { from, to, tokenId }
--             >>= awaitTxSuccessWeb3
--     owners <-
--       for transferV2Tokens
--         $ \tokenId ->
--             SuperRareV2.ownerOf
--               (defaultTxOpts primaryAccount # _to ?~ v2SuperRare.deployAddress)
--               Latest
--               { tokenId }
--     owners `shouldEqual` map Right transferToAddrs
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

-----------------------------------------------------------------------------
-- | Utils
-----------------------------------------------------------------------------
initSupeRareOld :: Aff (SupeRare.TestEnv ())
initSupeRareOld = do
  tenv@{ accounts, provider } <- SupeRare.init
  web3Test provider do
    whitelistAddresses tenv
    createOldSupeRareTokens tenv
  pure tenv
  where
  whitelistAddresses tenv@{ accounts } = void $ for accounts (SupeRare.whitelistAddress tenv)

  createOldSupeRareTokens tenv = void $ createTokensWithFunction tenv 2 (SupeRare.addNewToken tenv)

createTokensWithFunction ::
  forall r.
  { accounts :: Array Address | r } ->
  Int ->
  (Address -> String -> Web3 (UIntN S256)) ->
  Web3 (Array { tokenId :: UIntN S256, owner :: Address, uri :: String })
createTokensWithFunction { accounts } amount f = do
  tokenUris <- mkTokenUris (length accounts)
  for (zipWith { acc: _, _uri: _ } accounts tokenUris) \{ acc, _uri } -> do
    tokenId <- f acc _uri
    pure { owner: acc, uri: _uri, tokenId }

addNewToken :: forall r. TestEnv r -> Address -> String -> Web3 (UIntN S256)
addNewToken tenv@{ v2SuperRare: { deployAddress }, primaryAccount } from _uri = do
  SuperRareV2.addNewToken (defaultTxOpts from # _to ?~ deployAddress)
    { _uri }
    >>= awaitTxSuccessWeb3
  supply <- (unsafeToInt <<< unUIntN) <$> totalSupply tenv
  tokenByIndex tenv (intToUInt256 (supply - 1))

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

ownerOf :: forall r. TestEnv r -> UIntN S256 -> Web3 Address
ownerOf { v2SuperRare: { deployAddress }, primaryAccount } tokenId =
  throwOnCallError
    $ SuperRareV2.ownerOf
        (defaultTxOpts primaryAccount # _to ?~ deployAddress)
        Latest
        { tokenId }

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
