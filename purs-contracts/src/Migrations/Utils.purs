module Migrations.Utils where

import Prelude

import Chanterelle.Deploy (deployWithProvider)
import Chanterelle.Internal.Logging (LogLevel(..), log)
import Chanterelle.Internal.Types (DeployM)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (null)
import Deploy.Utils (GasSettings(..))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, try, Error, error)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Network.Ethereum.Web3 (CallError, Provider, httpProvider)
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

attempt ::
  forall e m a.
  MonadError e m =>
  MonadAff m =>
  Show e =>
  Int -> m a -> m a
attempt n f = do
  res <- try f
  case res of
    Left err ->
      if (n - 1) == 0 then
        liftEffect $ throw $ show err
      else do
        liftAff $ delay (Milliseconds 3000.0)
        log Warn $ "Errored with " <> show (n - 1) <> "attempts left.\n" <> show err
        attempt (n - 1) f
    Right v -> pure v

throwOnCallError :: forall a m. MonadThrow Error m => m (Either CallError a) -> m a
throwOnCallError f =
  f
    >>= case _ of
        Left cerr -> throwError $ error $ show cerr
        Right x -> pure x
