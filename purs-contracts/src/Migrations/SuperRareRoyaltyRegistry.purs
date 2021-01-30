module Migrations.SuperRareRoyaltyRegistry where

import Prelude
import Chanterelle.Internal.Types (DeployConfig(..), DeployM, throwDeploy)
import Contracts.SuperRareRoyaltyRegistry as SuperRareRoyaltyRegistry
import Control.Monad.Reader (ask)
import Data.Either (Either(..))
import Data.Lens ((?~))
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Deploy.Contracts.SuperRareRoyaltyRegistry (deployScriptWithGasSettings)
import Deploy.Utils (awaitTxSuccessAndLogEthStats, txOptsWithGasSettings)
import Effect (Effect)
import Effect.Aff (error)
import Effect.Aff.Class (liftAff)
import Migrations.Utils (MigrationSettings, emptyGasSettings, runMigration)
import Network.Ethereum.Core.HexString (HexString)
import Network.Ethereum.Web3 (Address, _from, _to, runWeb3)
import Network.Ethereum.Web3.Solidity.Sizes (s8)
import Partial.Unsafe (unsafePartial)
import Test.Spec.Contracts.Utils (intToUIntN)

type MigrationArgs
  = { iERC721TokenCreator :: Address }

type MigrationProgress
  = { superRareRoyaltyRegistry :: Maybe Address }

emptyMigrationProgress :: MigrationProgress
emptyMigrationProgress =
  { superRareRoyaltyRegistry: Nothing
  }

main :: Effect Unit
main = runMigration emptyMigrationProgress migration

migration :: MigrationSettings MigrationArgs MigrationProgress -> DeployM Unit
migration { migrationArgs: { iERC721TokenCreator }, getProgress, gasSettings: mgs, updateProgress } = do
  DeployConfig { provider, primaryAccount } <- ask
  let
    gasSettings = fromMaybe emptyGasSettings mgs
  deployContract gasSettings
  where
  -- Get the registry
  getSuperRareRoyaltyRegistry =
    getProgress
      >>= \{ superRareRoyaltyRegistry } ->
          pure $ unsafePartial fromJust superRareRoyaltyRegistry

  -- Deploy the contract
  deployContract gasSettings =
    getProgress
      >>= case _ of
          { superRareRoyaltyRegistry: Just _ } -> pure unit
          { superRareRoyaltyRegistry: Nothing } -> do
            { superRareRoyaltyRegistry: { deployAddress } } <- deployScriptWithGasSettings gasSettings { _iERC721TokenCreator: iERC721TokenCreator }
            updateProgress \prog -> prog { superRareRoyaltyRegistry = Just deployAddress }
            pure unit
