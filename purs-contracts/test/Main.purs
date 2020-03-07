module Test.Main where

import Prelude
import Chanterelle.Test (buildTestConfig)
import Deploy.Main (deployScript)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)

main :: Effect Unit
main =
  launchAff_
    $ do
        testConfig <- buildTestConfig "http://localhost:8545" 60 deployScript
        log "Wow we did it!"
