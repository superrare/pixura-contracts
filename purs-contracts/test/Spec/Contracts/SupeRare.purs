module Test.Spec.Contracts.SupeRare where

import Prelude
import Chanterelle.Test (buildTestConfig)
import Contracts.SupeRare (addNewToken, isWhitelisted, ownerOf, tokenURI, whitelistCreator) as SupeRare
import Data.Array (replicate, take, zip, (..))
import Data.Array.Partial (head)
import Data.Either (Either(..))
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Deploy.Contracts.SupeRare (deployScript) as SupeRare
import Deploy.Utils (awaitTxSuccessWeb3)
import Effect.Aff (Aff)
import Effect.Aff.AVar (put)
import Effect.Aff.Class (liftAff)
import Network.Ethereum.Core.BigNumber (decimal, embed, parseBigNumber)
import Network.Ethereum.Web3 (Address, ChainCursor(..), TransactionOptions, _from, _gas, _gasPrice, _to, defaultTransactionOptions, runWeb3, uIntNFromBigNumber)
import Network.Ethereum.Web3.Solidity.Sizes (s256)
import Network.Ethereum.Web3.Types (NoPay)
import Partial.Unsafe (unsafePartial)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Contracts.Utils (TestEnv, mkTokenUris, readOrFail)

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
spec testEnv@{ accounts: accsAV, provider: provAV, supeRare: srAV, supeRareTokens: srtAV } = do
  describe "SupeRare" do
    it "can deploy the contract" do
      sr <- liftAff $ buildTestConfig "http://localhost:8545" 60 SupeRare.deployScript
      put sr.supeRare srAV
      put (take 4 sr.accounts) accsAV
      put sr.provider provAV
    it "can whitelist accounts" do
      provider <- readOrFail provAV
      accounts <- readOrFail accsAV
      supeRare <- readOrFail srAV
      let
        runWeb3' = liftAff <<< runWeb3 provider

        acc1 = unsafePartial $ head accounts
      void
        $ runWeb3' do
            void
              $ traverse
                  ( \acc ->
                      SupeRare.whitelistCreator
                        (defaultTxOpts acc1 # _to ?~ supeRare.deployAddress)
                        { _creator: acc }
                        >>= awaitTxSuccessWeb3
                  )
                  accounts
            isWhitelistRess <-
              traverse
                ( \acc ->
                    SupeRare.isWhitelisted
                      (defaultTxOpts acc1 # _to ?~ supeRare.deployAddress)
                      Latest
                      { _creator: acc }
                )
                accounts
            isWhitelistRess `shouldEqual` replicate 4 (Right true)
    it "can mint tokens" do
      provider <- readOrFail provAV
      accounts <- readOrFail accsAV
      supeRare <- readOrFail srAV
      let
        runWeb3' = liftAff <<< runWeb3 provider

        acc1 = unsafePartial $ head accounts

        tokenIds = map (\tid -> unsafePartial fromJust $ uIntNFromBigNumber s256 $ embed tid) (1 .. 4)
      void
        $ runWeb3' do
            tokenUris <- mkTokenUris 4
            void
              $ traverse
                  ( \(Tuple acc _uri) ->
                      SupeRare.addNewToken (defaultTxOpts acc # _to ?~ supeRare.deployAddress)
                        { _uri }
                        >>= awaitTxSuccessWeb3
                  )
                  (zip accounts tokenUris)
            owners <-
              traverse
                ( \tid ->
                    SupeRare.ownerOf
                      (defaultTxOpts acc1 # _to ?~ supeRare.deployAddress)
                      Latest
                      { _tokenId: unsafePartial fromJust $ uIntNFromBigNumber s256 $ embed tid }
                )
                (1 .. 4)
            owners `shouldEqual` map Right accounts
            uris <-
              traverse
                ( \tid ->
                    SupeRare.tokenURI
                      (defaultTxOpts acc1 # _to ?~ supeRare.deployAddress)
                      Latest
                      { _tokenId: tid }
                )
                tokenIds
            uris `shouldEqual` map Right tokenUris
            liftAff $ put tokenIds srtAV
