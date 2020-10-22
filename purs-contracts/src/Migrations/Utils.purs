module Migrations.Utils where

import Prelude
import Chanterelle.Deploy (deployWithProvider)
import Chanterelle.Internal.Logging (LogLevel(..), log)
import Chanterelle.Internal.Types (DeployConfig(..), DeployM, throwDeploy)
import Control.Comonad.Env (ask)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError, try)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable (null)
import Deploy.Utils (GasSettings(..), txOptsWithGasSettings)
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.Aff (Aff, Error, Milliseconds(..), delay, error, runAff_, try)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (throw, throwException)
import Network.Ethereum.Web3 (CallError, Provider, TransactionOptions(..), httpProvider)
import Network.Ethereum.Web3.Types.HdWalletProvider (hdWalletProvider, unHdWalletProvider)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Node.Process (cwd, lookupEnv)
import Simple.JSON as JSON

runMigration ::
  forall a b c.
  JSON.ReadForeign b =>
  JSON.ReadForeign c =>
  JSON.WriteForeign c =>
  c ->
  ( MigrationSettings b c ->
    DeployM a
  ) ->
  Effect Unit
runMigration emptyMigrationProgress migration =
  runAff_ (either throwException (const $ log Info "Completed Migration")) do
    config@{ migrationArgs, gasSettings, progressFile } <- loadMigrationConfig
    provider <- liftEffect $ mkProvider config
    ep <- try $ readJSONFile progressFile
    let
      migrationProgress = case ep of
        Left _ -> emptyMigrationProgress
        Right mp -> mp
    avMp <- AVar.new migrationProgress
    emigration <- try $ deployWithProvider provider (60 * 1000) (migration { migrationArgs, gasSettings, getProgress: getAVar avMp, updateProgress: updateAVar progressFile avMp })
    writeProgressFile progressFile avMp
    case emigration of
      Left err -> do
        log Error $ "Failed to complete migration with error: " <> show err
        throwError err
      Right _ -> pure unit
  where
  writeProgressFile progressFile avMp = do
    mmd <- AVar.tryRead avMp
    case mmd of
      Nothing -> throwError (error "Migration Progress found to be empty")
      Just md -> writeTextFile UTF8 progressFile (JSON.writeJSON md)

  getAVar avMp = liftAff $ AVar.read avMp

  updateAVar progressFile avMp f =
    liftAff do
      mp <- AVar.take avMp
      AVar.put (f mp) avMp
      writeProgressFile progressFile avMp

readJSONFile :: forall t3. JSON.ReadForeign t3 => String -> Aff t3
readJSONFile n = do
  t <- readTextFile UTF8 n
  case JSON.readJSON t of
    Left err -> throwError (error $ show err)
    Right v -> pure v

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
    , progressFile :: String
    }

type MigrationSettings b c
  = { migrationArgs :: b
    , getProgress :: DeployM c
    , updateProgress :: (c -> c) -> DeployM Unit
    , gasSettings ::
        Maybe GasSettings
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
