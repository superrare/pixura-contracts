module Test.Spec.Contracts.SuperRareV2 where

import Prelude
import Chanterelle.Test (buildTestConfig)
import Contracts.SuperRareV2 (addNewToken, addToWhitelist, isWhitelisted, ownerOf, tokenURI) as SuperRareV2
import Data.Array (replicate, zip, (..))
import Data.Array.Partial (head, last)
import Data.Either (Either(..))
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Deploy.Contracts.SuperRareV2 (deployScript) as SuperRareV2
import Deploy.Utils (awaitTxSuccessWeb3)
import Effect.Aff (Aff)
import Effect.Aff.AVar (put)
import Effect.Aff.Class (liftAff)
import Network.Ethereum.Core.BigNumber (decimal, embed, parseBigNumber, unsafeToInt)
import Network.Ethereum.Web3 (Address, ChainCursor(..), TransactionOptions, _from, _gas, _gasPrice, _to, defaultTransactionOptions, runWeb3, uIntNFromBigNumber, unUIntN)
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
spec { superRareV2: sr2AV
, accounts: accsAV
, provider: provAV
, supeRare: srAV
, supeRareTokens: srtAV
, superRareV2Tokens: srV2TAV
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
      put sr.superRareV2 sr2AV
    it "can whitelist accounts" do
      provider <- readOrFail provAV
      accounts <- readOrFail accsAV
      superRareV2@{ deployAddress } <- readOrFail sr2AV
      let
        runWeb3' = liftAff <<< runWeb3 provider

        acc1 = unsafePartial $ head accounts
      void
        $ runWeb3' do
            void
              $ traverse
                  ( \acc ->
                      SuperRareV2.addToWhitelist
                        (defaultTxOpts acc1 # _to ?~ deployAddress)
                        { _newAddress: acc }
                        >>= awaitTxSuccessWeb3
                  )
                  accounts
            isWhitelistRess <-
              traverse
                ( \acc ->
                    SuperRareV2.isWhitelisted
                      (defaultTxOpts acc1 # _to ?~ deployAddress)
                      Latest
                      { _address: acc }
                )
                accounts
            isWhitelistRess `shouldEqual` replicate 4 (Right true)
    it "can mint tokens" do
      provider <- readOrFail provAV
      accounts <- readOrFail accsAV
      srTokens <- readOrFail srtAV
      superRareV2@{ deployAddress } <- readOrFail sr2AV
      let
        runWeb3' = liftAff <<< runWeb3 provider

        acc1 = unsafePartial $ head accounts

        lastId = unsafeToInt $ unUIntN $ unsafePartial $ last srTokens

        tokenIds = map (\tid -> unsafePartial fromJust $ uIntNFromBigNumber s256 $ embed tid) ((lastId + 1) .. (lastId + 4))
      void
        $ runWeb3' do
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
                      (defaultTxOpts acc1 # _to ?~ deployAddress)
                      Latest
                      { tokenId }
                )
                tokenIds
            owners `shouldEqual` map Right accounts
            uris <-
              traverse
                ( \tokenId ->
                    SuperRareV2.tokenURI
                      (defaultTxOpts acc1 # _to ?~ deployAddress)
                      Latest
                      { tokenId }
                )
                tokenIds
            uris `shouldEqual` map Right tokenUris
            liftAff $ put tokenIds srV2TAV
 {-
    -- set price
    -- buy
    -- bid
    -- out bid
    -- accept bid
    -- transfer with bid
    -- transfer with set price
    deploy SR New
    deploy SR AuctionMarket
    -- appove marketauction 
    -- whitelist 4 same addresses
    -- mint 4 more tokens
    -- set price
    -- buy
    -- bid
    -- out bid
    -- accept bid
    -- transfer with bid
    -- transfer with set price
    deploy SR AuctionMarketV2
    -- appove marketauctionv2
    -- mark previously sold tokens as sold
    -- set price
    -- buy
    -- bid
    -- accept bid
    deploy hack
    -- make hack bid on V2 Marketplace
    -- outbid hack bid on V2 Marketplace
    -- 
    -- 
    -- 
        
        
-}