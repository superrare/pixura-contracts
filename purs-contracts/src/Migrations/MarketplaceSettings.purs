module Migrations.MarketplaceSettings where

import Prelude
import Chanterelle.Internal.Types (DeployConfig(..), DeployM, throwDeploy)
import Contracts.Marketplace.MarketplaceSettings as MarketplaceSettings
import Control.Monad.Reader (ask)
import Data.Either (Either(..))
import Data.Lens ((?~))
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Deploy.Contracts.MarketplaceSettings (deployScriptWithGasSettings)
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
  = { marketSettingsAddress :: Maybe Address
    , setSuperRareV2PrimarySaleFeeTx :: Maybe HexString
    }

emptyMigrationProgress :: MigrationProgress
emptyMigrationProgress =
  { marketSettingsAddress: Nothing
  , setSuperRareV2PrimarySaleFeeTx: Nothing
  }

main :: Effect Unit
main = runMigration emptyMigrationProgress migration

migration :: MigrationSettings MigrationArgs MigrationProgress -> DeployM Unit
migration { migrationArgs, getProgress, gasSettings: mgs, updateProgress } = do
  DeployConfig { provider, primaryAccount } <- ask
  let
    gasSettings = fromMaybe emptyGasSettings mgs
  deployContract gasSettings
  setSuperRareV2PrimarySaleFee gasSettings
  where
  -- Get the market settings address
  getMarketSettingsAddress = getProgress >>= \{ marketSettingsAddress } -> pure $ unsafePartial fromJust marketSettingsAddress

  -- Deploy the contract
  deployContract gasSettings =
    getProgress
      >>= case _ of
          { marketSettingsAddress: Just _ } -> pure unit
          { marketSettingsAddress: Nothing } -> do
            { marketplaceSettings: { deployAddress } } <- deployScriptWithGasSettings gasSettings
            updateProgress \prog -> prog { marketSettingsAddress = Just deployAddress }
            pure unit

  -- Set the Primary sale fee for V2 SuperRare contracts
  setSuperRareV2PrimarySaleFee gasSettings =
    getProgress
      >>= case _ of
          { setSuperRareV2PrimarySaleFeeTx: Just _ } -> pure unit
          { setSuperRareV2PrimarySaleFeeTx: Nothing } -> do
            marketSettingsAddress <- getMarketSettingsAddress
            DeployConfig { provider, primaryAccount } <- ask
            let
              { v2SuperRareAddress } = migrationArgs

              txOpts =
                txOptsWithGasSettings gasSettings
                  # _from
                  ?~ primaryAccount
                  # _to
                  ?~ marketSettingsAddress
            etxHash <-
              liftAff
                $ runWeb3 provider do
                    txHash <-
                      MarketplaceSettings.setERC721ContractPrimarySaleFeePercentage
                        txOpts
                        { _contractAddress: v2SuperRareAddress, _percentage: intToUIntN s8 15 }
                    awaitTxSuccessAndLogEthStats txHash
                    pure txHash
            case etxHash of
              Left err ->
                throwDeploy $ error
                  $ "Failed setting SuperRareV2 primary sale percentage with error: "
                  <> show err
              Right txHash -> updateProgress \prog -> prog { setSuperRareV2PrimarySaleFeeTx = Just txHash }
