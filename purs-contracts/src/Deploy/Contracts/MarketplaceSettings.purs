module Deploy.Contracts.MarketplaceSettings where

import Prelude
import Chanterelle.Internal.Deploy (DeployReceipt)
import Chanterelle.Internal.Types (ContractConfig, DeployM, NoArgs, constructorNoArgs, noArgs)
import Deploy.Utils (GasSettings, deployContractWithConfig)
import Migrations.Utils (emptyGasSettings)

makeMarketplaceSettingsConfig :: ContractConfig NoArgs
makeMarketplaceSettingsConfig =
  { filepath: "./contracts/build/MarketplaceSettings.json"
  , name: "MarketplaceSettings"
  , constructor: constructorNoArgs
  , unvalidatedArgs: noArgs
  }

type DeployResults
  = ( marketplaceSettings :: DeployReceipt NoArgs
    )

deployScript ::
  DeployM (Record DeployResults)
deployScript = deployScriptWithGasSettings emptyGasSettings

deployScriptWithGasSettings ::
  GasSettings ->
  DeployM (Record DeployResults)
deployScriptWithGasSettings gasSettings = do
  marketplaceSettings <-
    deployContractWithConfig
      { contractConfig: makeMarketplaceSettingsConfig
      , gasSettings
      }
  pure { marketplaceSettings }
