module Test.Main where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), launchAff_)
import Test.Spec.Contracts.SupeRare as SupeRare
import Test.Spec.Contracts.SuperRareV2 as SuperRareV2
import Test.Spec.Contracts.SuperRareMarketAuctionV2 as SuperRareMarketAuctionV2
import Test.Spec.Contracts.Utils (init)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpecT, defaultConfig)

main :: Effect Unit
main =
  launchAff_
    $ do
        let
          specConfig = defaultConfig { timeout = Just (Milliseconds $ 120.0 * 1000.0) }
        join
          $ runSpecT specConfig [ consoleReporter ] do
              SupeRare.spec
 -- _ <- SuperRareV2.spec tenv -- void $ SuperRareMarketAuctionV2.spec tenv