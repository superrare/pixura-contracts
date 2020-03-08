module Test.Main where

import Prelude

import Chanterelle.Test (buildTestConfig)
import Contracts.SupeRare as SupeRare
import Control.Monad.List.Trans (take)
import Data.Array ((!!))
import Data.Lens ((?~))
import Data.Maybe (Maybe(..), fromJust)
import Data.Traversable (traverse)
import Deploy.Contracts.SupeRare as SupeRare
import Deploy.Contracts.SuperRareMarketAuction as SuperRareMarketAuction
import Deploy.Contracts.SuperRareMarketAuctionV2 as SuperRareMarketAuctionV2
import Deploy.Contracts.SuperRareV2 as SuperRareV2
import Effect (Effect)
import Effect.Aff (Milliseconds(..), launchAff_)
import Effect.Class.Console (log)
import Foreign.Index ((!))
import Network.Ethereum.Core.BigNumber (decimal, parseBigNumber)
import Network.Ethereum.Web3 (Address, TransactionOptions(..), _from, _gas, _gasPrice, defaultTransactionOptions, runWeb3)
import Network.Ethereum.Web3.Types (NoPay)
import Partial.Unsafe (unsafePartial)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpecT, defaultConfig)
import Test.Spec.E2E as E2E


defaultTxOpts ::  Address -> TransactionOptions NoPay
defaultTxOpts primaryAccount = 
  let limit = unsafePartial fromJust $ parseBigNumber decimal "6712388"
      price = unsafePartial fromJust $ parseBigNumber decimal "10000000000"
  in defaultTransactionOptions # _from ?~ primaryAccount
                               # _gas ?~ limit
                               # _gasPrice ?~ price 

main :: Effect Unit
main = launchAff_ $ do
  let specConfig = defaultConfig {timeout = Just (Milliseconds $ 120.0 * 1000.0)}
  join $ runSpecT specConfig [consoleReporter] do
    E2E.spec