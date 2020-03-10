module Test.Spec.Contracts.SuperRareV2 where

import Prelude
import Chanterelle.Test (buildTestConfig)
import Contracts.SuperRareV2 (addNewToken, addToWhitelist, isWhitelisted, ownerOf, tokenURI, transferFrom) as SuperRareV2
import Data.Array (elem, filter, replicate, take, zip, (..))
import Data.Array.Partial (head, last)
import Data.Either (Either(..), fromRight)
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..), fst)
import Deploy.Contracts.SuperRareV2 (deployScript) as SuperRareV2
import Deploy.Utils (awaitTxSuccessWeb3)
import Effect.Aff (Aff)
import Effect.Aff.AVar (put)
import Effect.Aff.Class (liftAff)
import Network.Ethereum.Core.BigNumber (decimal, embed, parseBigNumber, unsafeToInt)
import Network.Ethereum.Web3 (Address, ChainCursor(..), TransactionOptions, _from, _gas, _gasPrice, _to, defaultTransactionOptions, uIntNFromBigNumber, unUIntN)
import Network.Ethereum.Web3.Solidity.Sizes (s256)
import Network.Ethereum.Web3.Types (NoPay)
import Partial.Unsafe (unsafePartial)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Contracts.Utils (TestEnv, mkTokenUris, readOrFail, web3Test)

defaultTxOpts :: Address -> TransactionOptions NoPay
defaultTxOpts primaryAccount =
  let
    limit = unsafePartial fromJust $ parseBigNumber decimal "6712388"

    price = unsafePartial fromJust $ parseBigNumber decimal "10000000000"
  in
    defaultTransactionOptions # _from ?~ primaryAccount
      # _gas
      ?~ limit
      # _gasPrice
      ?~ price

spec :: TestEnv -> SpecT Aff Unit Aff Unit
spec { superRareV2: v2SuperRareAV
, accounts: accsAV
, provider: provAV
, supeRare: srAV
, supeRareTokens: srTokensAV
, superRareV2Tokens: v2TokensAV
, primaryAccount: primAccAv
} = do
  describe "SuperRareV2" do
    it "can deploy the contract" do
      supeRare <- readOrFail srAV
      sr <-
        liftAff $ buildTestConfig "http://localhost:8545" 60
          $ SuperRareV2.deployScript
              { _name: "SuperRareV2"
              , _symbol: "SUPR"
              , _oldSuperRare: supeRare.deployAddress
              }
      put sr.superRareV2 v2SuperRareAV
    it "can whitelist accounts" do
      provider <- readOrFail provAV
      accounts <- readOrFail accsAV
      superRareV2@{ deployAddress } <- readOrFail v2SuperRareAV
      primAcc <- readOrFail primAccAv
      web3Test provider do
        void
          $ traverse
              ( \acc ->
                  SuperRareV2.addToWhitelist
                    (defaultTxOpts primAcc # _to ?~ deployAddress)
                    { _newAddress: acc }
                    >>= awaitTxSuccessWeb3
              )
              accounts
        isWhitelistRess <-
          traverse
            ( \acc ->
                SuperRareV2.isWhitelisted
                  (defaultTxOpts primAcc # _to ?~ deployAddress)
                  Latest
                  { _address: acc }
            )
            accounts
        isWhitelistRess `shouldEqual` replicate 4 (Right true)
    it "can mint tokens" do
      provider <- readOrFail provAV
      accounts <- readOrFail accsAV
      srTokens <- readOrFail srTokensAV
      superRareV2@{ deployAddress } <- readOrFail v2SuperRareAV
      primAcc <- readOrFail primAccAv
      let
        lastId = unsafeToInt $ unUIntN $ unsafePartial $ last srTokens

        tokenIds = map (\tid -> unsafePartial fromJust $ uIntNFromBigNumber s256 $ embed tid) ((lastId + 1) .. (lastId + 4))
      web3Test provider do
        tokenUris <- mkTokenUris 4
        void
          $ traverse
              ( \(Tuple acc _uri) ->
                  SuperRareV2.addNewToken (defaultTxOpts acc # _to ?~ deployAddress)
                    { _uri }
                    >>= awaitTxSuccessWeb3
              )
              (zip accounts tokenUris)
        owners <-
          traverse
            ( \tokenId ->
                SuperRareV2.ownerOf
                  (defaultTxOpts primAcc # _to ?~ deployAddress)
                  Latest
                  { tokenId }
            )
            tokenIds
        owners `shouldEqual` map Right accounts
        uris <-
          traverse
            ( \tokenId ->
                SuperRareV2.tokenURI
                  (defaultTxOpts primAcc # _to ?~ deployAddress)
                  Latest
                  { tokenId }
            )
            tokenIds
        uris `shouldEqual` map Right tokenUris
        liftAff $ put tokenIds v2TokensAV
    it "can transfer tokens" do
      provider <- readOrFail provAV
      accounts <- readOrFail accsAV
      v2SuperRare <- readOrFail v2SuperRareAV
      v2Tokens <- readOrFail v2TokensAV
      primAcc <- readOrFail primAccAv
      let
        transferV2Tokens = (take 2 v2Tokens)
      web3Test provider do
        ownerAndTokens <-
          for transferV2Tokens
            $ \tokenId -> do
                owner <-
                  map (unsafePartial fromRight)
                    $ SuperRareV2.ownerOf
                        (defaultTxOpts primAcc # _to ?~ v2SuperRare.deployAddress)
                        Latest
                        { tokenId }
                pure (Tuple owner tokenId)
        let
          transferToAddrs = filter (\acc -> not $ elem acc (map fst ownerAndTokens)) accounts

          transferPayloads = zip ownerAndTokens transferToAddrs
        void $ for transferPayloads
          $ \(Tuple (Tuple from tokenId) to) -> do
              SuperRareV2.transferFrom
                (defaultTxOpts from # _to ?~ v2SuperRare.deployAddress)
                { from, to, tokenId }
                >>= awaitTxSuccessWeb3
        owners <-
          for transferV2Tokens
            $ \tokenId ->
                SuperRareV2.ownerOf
                  (defaultTxOpts primAcc # _to ?~ v2SuperRare.deployAddress)
                  Latest
                  { tokenId }
        owners `shouldEqual` map Right transferToAddrs
