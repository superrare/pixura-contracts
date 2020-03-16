module Migrations.Utils where

import Prelude
import Chanterelle.Deploy (deployWithProvider)
import Chanterelle.Internal.Types (DeployM)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (null)
import Deploy.Utils (GasSettings(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Network.Ethereum.Web3 (Provider, httpProvider)
import Network.Ethereum.Web3.Types.HdWalletProvider (hdWalletProvider, unHdWalletProvider)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Process (lookupEnv)
import Simple.JSON as JSON

runMigration ::
  forall a b.
  JSON.ReadForeign b =>
  ( { migrationArgs :: b
    , gasSettings ::
        Maybe GasSettings
    } ->
    DeployM a
  ) ->
  Aff a
runMigration migration = do
  config@{ migrationArgs, gasSettings } <- loadMigrationConfig
  provider <- liftEffect $ mkProvider config
  deployWithProvider provider (60 * 1000) (migration { migrationArgs, gasSettings })

loadMigrationConfig :: forall a. JSON.ReadForeign a => Aff (MigrationConfig a)
loadMigrationConfig = do
  cfgPath <- maybe (liftEffect $ throw "`CONFIG` environment variable not found") pure =<< (liftEffect $ lookupEnv "CONFIG")
  contents <- readTextFile UTF8 cfgPath
  case JSON.readJSON contents of
    Left err -> liftEffect $ throw $ show err
    Right a -> pure a

emptyGasSettings :: GasSettings
emptyGasSettings = GasSettings { gasPrice: Nothing, gasLimit: Nothing }

mkProvider :: forall a. (MigrationConfig (a)) -> Effect Provider
mkProvider cfg@{ rpcUrl } =
  liftEffect case cfg of
    { mnemonic: Just mnemonic } ->
      unHdWalletProvider
        <$> hdWalletProvider { mnemonic, rpc: rpcUrl, path: null, numberOfAccounts: null }
    _ -> httpProvider rpcUrl

type MigrationConfig a
  = { rpcUrl :: String
    , mnemonic :: Maybe String
    , gasSettings :: Maybe GasSettings
    , migrationArgs :: a
    }
