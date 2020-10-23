module Migrations.SuperRareTokenCreatorRegistry where

import Prelude
import Chanterelle.Internal.Types (DeployConfig(..), DeployM, throwDeploy)
import Contracts.SuperRareTokenCreatorRegistry as SuperRareTokenCreatorRegistry
import Control.Monad.Reader (ask)
import Data.Either (Either(..))
import Data.Lens ((?~))
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Deploy.Contracts.SuperRareTokenCreatorRegistry (deployScriptWithGasSettings)
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
  = { v2SuperRareAddress :: Address }

type MigrationProgress
  = { superRareTokenCreatorRegistry :: Maybe Address }

emptyMigrationProgress :: MigrationProgress
emptyMigrationProgress =
  { superRareTokenCreatorRegistry: Nothing
  }

main :: Effect Unit
main = runMigration emptyMigrationProgress migration

migration :: MigrationSettings MigrationArgs MigrationProgress -> DeployM Unit
migration { migrationArgs: { v2SuperRareAddress }, getProgress, gasSettings: mgs, updateProgress } = do
  DeployConfig { provider, primaryAccount } <- ask
  let
    gasSettings = fromMaybe emptyGasSettings mgs
  deployContract gasSettings
  where
  -- Get the registry
  getSuperRareTokenCreatorRegistry =
    getProgress
      >>= \{ superRareTokenCreatorRegistry } ->
          pure $ unsafePartial fromJust superRareTokenCreatorRegistry

  -- Deploy the contract
  deployContract gasSettings =
    getProgress
      >>= case _ of
          { superRareTokenCreatorRegistry: Just _ } -> pure unit
          { superRareTokenCreatorRegistry: Nothing } -> do
            { superRareTokenCreatorRegistry: { deployAddress } } <- deployScriptWithGasSettings gasSettings { _iERC721Creators: [ v2SuperRareAddress ] }
            updateProgress \prog -> prog { superRareTokenCreatorRegistry = Just deployAddress }
            pure unit
