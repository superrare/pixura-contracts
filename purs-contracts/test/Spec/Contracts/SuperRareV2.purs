module Test.Spec.Contracts.SuperRareV2 where

import Prelude
import Chanterelle.Test (buildTestConfig)
import Contracts.SuperRareV2 (addNewToken, addToWhitelist, isWhitelisted, ownerOf, tokenURI) as SuperRareV2
import Data.Array (replicate, zip, (..))
import Data.Array.Partial (head)
import Data.Either (Either(..))
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Deploy.Contracts.SuperRareV2 (deployScript) as SuperRareV2
import Deploy.Utils (awaitTxSuccessWeb3)
import Effect.Aff (Aff)
import Effect.Aff.AVar (put, read)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Network.Ethereum.Core.BigNumber (decimal, embed, parseBigNumber)
import Network.Ethereum.Web3 (Address, ChainCursor(..), TransactionOptions, _from, _gas, _gasPrice, _to, defaultTransactionOptions, runWeb3, uIntNFromBigNumber)
import Network.Ethereum.Web3.Solidity.Sizes (s256)
import Network.Ethereum.Web3.Types (NoPay)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (randomSample')
import Test.Spec (SpecT, beforeAll, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Contracts.Utils (init)

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

spec :: SpecT Aff Unit Aff Unit
spec =
  beforeAll init
    $ do
        describe "SuperRareV2"
          $ do
              it "can deploy the contract"
                $ \{ superRareV2, supeRare: srAV } -> do
                    supeRare <- read srAV
                    sr <-
                      liftAff $ buildTestConfig "http://localhost:8545" 60
                        $ SuperRareV2.deployScript
                            { _name: "SuperRareV2"
                            , _symbol: "SUPR"
                            , _oldSuperRare: supeRare.deployAddress
                            }
                    put sr.superRareV2 superRareV2
              it "can whitelist accounts"
                $ \{ superRareV2: sr2AV, accounts: accsAV, provider: provAV } -> do
                    provider <- read provAV
                    accounts <- read accsAV
                    superRareV2@{ deployAddress } <- read sr2AV
                    let
                      runWeb3' = liftAff <<< runWeb3 provider

                      acc1 = unsafePartial $ head accounts
                    void $ runWeb3'
                      $ do
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
              it "can mint tokens"
                $ \{ superRareV2: sr2AV, accounts: accsAV, provider: provAV } -> do
                    provider <- read provAV
                    accounts <- read accsAV
                    superRareV2@{ deployAddress } <- read sr2AV
                    let
                      runWeb3' = liftAff <<< runWeb3 provider

                      acc1 = unsafePartial $ head accounts
                    void $ runWeb3'
                      $ do
                          tokenUris <- liftEffect $ randomSample' 4 arbitrary
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
                              ( \tid ->
                                  SuperRareV2.ownerOf
                                    (defaultTxOpts acc1 # _to ?~ deployAddress)
                                    Latest
                                    { tokenId: unsafePartial fromJust $ uIntNFromBigNumber s256 $ embed tid }
                              )
                              (1 .. 4)
                          owners `shouldEqual` map Right accounts
                          uris <-
                            traverse
                              ( \tid ->
                                  SuperRareV2.tokenURI
                                    (defaultTxOpts acc1 # _to ?~ deployAddress)
                                    Latest
                                    { tokenId: unsafePartial fromJust $ uIntNFromBigNumber s256 $ embed tid }
                              )
                              (1 .. 4)
                          uris `shouldEqual` map Right tokenUris
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