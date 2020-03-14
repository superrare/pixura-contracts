module Migrations.SuperRareMarketAuctionV2 where

import Prelude
import Chanterelle.Deploy (deployWithProvider)
import Chanterelle.Internal.Types (DeployConfig(..), DeployM(..), NoArgs)
import Contracts.V5.SuperRareMarketAuctionV2 (markTokensAsSold)
import Control.Monad.Except (runExcept)
import Control.Monad.Reader (ask)
import Data.Array (drop, take, (:))
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable (null)
import Data.Traversable (for)
import Deploy.Contracts.SuperRareMarketAuctionV2 (deployScriptWithGasSettings)
import Deploy.Utils (GasSettings, awaitTxSuccessWeb3, defaultTxOptions, txOptsWithGasSettings)
import Effect (Effect)
import Effect.Aff (joinFiber, launchAff, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Effect.Exception (throw)
import Migrations.Utils (emptyGasSettings, runMigration)
import Network.Ethereum.Web3 (Address, httpProvider, runWeb3)
import Network.Ethereum.Web3.Types.HdWalletProvider (hdWalletProvider, unHdWalletProvider)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Process (lookupEnv)
import Simple.JSON as JSON

type MigrationArgs
  = { superRareV2ContractAddress :: Address
    }

main :: Effect Unit
main =
  launchAff_
    $ runMigration \(args :: { gasSettings :: Maybe GasSettings, migrationArgs :: MigrationArgs }) -> do
        let
          { migrationArgs: { superRareV2ContractAddress: _originContract }
          , gasSettings: mgs
          } = args

          txOpts = txOptsWithGasSettings gasSettings

          gasSettings = fromMaybe emptyGasSettings mgs
        DeployConfig { provider, primaryAccount } <- ask
        fibTokens <- liftEffect $ launchAff (lookUpSoldTokens _originContract)
        { superRareMarketAuctionV2 } <- deployScriptWithGasSettings gasSettings
        res <-
          liftAff
            $ runWeb3 provider do
                soldTokens <- liftAff $ joinFiber fibTokens
                for (chunk 100 soldTokens) \_tokenIds ->
                  markTokensAsSold txOpts { _originContract, _tokenIds } >>= awaitTxSuccessWeb3
        case res of
          Left err -> liftEffect $ throw $ show err
          _ -> pure unit
  where
  chunk n [] = []

  chunk n xs = take n xs : chunk n (drop n xs)

  lookUpSoldTokens _ = pure []
