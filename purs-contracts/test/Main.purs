module Test.Main where

import Prelude
import Data.Lens ((?~))
import Data.Maybe (Maybe(..), fromJust)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), launchAff_)
import Network.Ethereum.Core.BigNumber (decimal, parseBigNumber)
import Network.Ethereum.Web3 (Address, TransactionOptions, _from, _gas, _gasPrice, defaultTransactionOptions)
import Network.Ethereum.Web3.Types (NoPay)
import Partial.Unsafe (unsafePartial)
import Test.Spec.Contracts.SupeRare as SupeRare
import Test.Spec.Contracts.SuperRareV2 as SuperRareV2
import Test.Spec.Contracts.Utils (init)
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

main :: Effect Unit
main =
  launchAff_
    $ do
        let
          specConfig = defaultConfig { timeout = Just (Milliseconds $ 120.0 * 1000.0) }
        tenv <- init
        join
          $ runSpecT specConfig [ consoleReporter ] do
              SupeRare.spec tenv
              SuperRareV2.spec tenv
