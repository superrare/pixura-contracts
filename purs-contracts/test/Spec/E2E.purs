module Test.Spec.E2E where

import Prelude
import Chanterelle.Internal.Deploy (DeployReceipt)
import Chanterelle.Internal.Logging (LogLevel(..))
import Chanterelle.Internal.Types (NoArgs)
import Chanterelle.Test (buildTestConfig)
import Contracts.SupeRare as SupeRare
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.List.Trans (lift, repeat, take)
import Data.Array (replicate, zip, (!!), (..))
import Data.Either (Either(..))
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Deploy.Contracts.SupeRare as SupeRare
import Deploy.Contracts.SuperRareMarketAuction as SuperRareMarketAuction
import Deploy.Contracts.SuperRareMarketAuctionV2 as SuperRareMarketAuctionV2
import Deploy.Contracts.SuperRareV2 as SuperRareV2
import Deploy.Utils (awaitTxSuccessWeb3)
import Effect (Effect)
import Effect.Aff.AVar (AVar, empty, new)
import Effect.Aff (Aff, Error, launchAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Foreign.Index ((!))
import Network.Ethereum.Core.BigNumber (decimal, embed, parseBigNumber)
import Network.Ethereum.Web3 (Address, CallError, ChainCursor(..), TransactionOptions(..), UIntN, _from, _gas, _gasPrice, _to, defaultTransactionOptions, runWeb3, uIntNFromBigNumber)
import Network.Ethereum.Web3.Solidity.Sizes (S256, s256)
import Network.Ethereum.Web3.Types (NoPay)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (randomSample')
import Test.Spec (SpecT(..), before, beforeAll, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpecT, defaultConfig)

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

spec ::
  forall m.
  MonadAff m =>
  MonadError Error m =>
  SpecT Aff Unit Aff Unit
spec =
  beforeAll init
    $ do
        describe "e2e test"
          $ do
              it "can pass all e2e tests"
                $ \_ -> do
                    -- deploy SROld
                    { accounts, provider, supeRare } <- liftAff $ buildTestConfig "http://localhost:8545" 60 SupeRare.deployScript
                    let
                      runWeb3' = liftAff <<< runWeb3 provider

                      acc1 = unsafePartial fromJust $ accounts !! 0

                      acc2 = unsafePartial fromJust $ accounts !! 1

                      acc3 = unsafePartial fromJust $ accounts !! 2

                      acc4 = unsafePartial fromJust $ accounts !! 3

                      accs = [ acc1, acc2, acc3, acc4 ]
                    void $ runWeb3'
                      $ do
                          -- whitelist 4 addresses
                          txHashes <-
                            traverse
                              ( \acc ->
                                  SupeRare.whitelistCreator
                                    (defaultTxOpts acc1 # _to ?~ supeRare.deployAddress)
                                    { _creator: acc }
                              )
                              accs
                          void $ traverse awaitTxSuccessWeb3 txHashes
                          isWhitelistRess <-
                            traverse
                              ( \acc ->
                                  SupeRare.isWhitelisted
                                    (defaultTxOpts acc1 # _to ?~ supeRare.deployAddress)
                                    Latest
                                    { _creator: acc }
                              )
                              accs
                          isWhitelistRess `shouldEqual` replicate 4 (Right true)
                          -- mint 4 tokens
                          tokenUris <- liftEffect $ randomSample' 4 arbitrary
                          void
                            $ traverse
                                ( \(Tuple acc _uri) ->
                                    SupeRare.addNewToken (defaultTxOpts acc # _to ?~ supeRare.deployAddress)
                                      { _uri }
                                      >>= awaitTxSuccessWeb3
                                )
                                (zip accs tokenUris)
                          owners <-
                            traverse
                              ( \tid ->
                                  SupeRare.ownerOf
                                    (defaultTxOpts acc1 # _to ?~ supeRare.deployAddress)
                                    Latest
                                    { _tokenId: unsafePartial fromJust $ uIntNFromBigNumber s256 $ embed tid }
                              )
                              (1 .. 4)
                          owners `shouldEqual` map Right accs

init :: Aff TestEnv
init =
  { supeRare: _
  , superRareV2: _
  , superRareMarketAuction: _
  , superRareMarketAuctionV2: _
  , supeRareTokens: _
  }
    <$> empty
    <*> empty
    <*> empty
    <*> empty
    <*> empty

type TestEnv
  = { supeRare :: AVar (DeployReceipt NoArgs)
    , superRareV2 :: AVar (DeployReceipt SuperRareV2.SuperRareV2)
    , superRareMarketAuction :: AVar (DeployReceipt NoArgs)
    , superRareMarketAuctionV2 :: AVar (DeployReceipt NoArgs)
    , supeRareTokens :: AVar (Array (UIntN S256))
    }
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