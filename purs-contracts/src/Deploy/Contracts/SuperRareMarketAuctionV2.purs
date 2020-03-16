module Deploy.Contracts.SuperRareMarketAuctionV2 where

import Prelude
import Chanterelle.Internal.Deploy (DeployReceipt)
import Chanterelle.Internal.Types (ContractConfig, DeployM, NoArgs, constructorNoArgs, noArgs)
import Deploy.Utils (GasSettings, deployContractWithConfig)
import Migrations.Utils (emptyGasSettings)

makeSuperRareMarketAuctionV2Config :: ContractConfig NoArgs
makeSuperRareMarketAuctionV2Config =
  { filepath: "./contracts/v5/build/SuperRareMarketAuctionV2.json"
  , name: "SuperRareMarketAuctionV2"
  , constructor: constructorNoArgs
  , unvalidatedArgs: noArgs
  }

type DeployResults
  = ( superRareMarketAuctionV2 :: DeployReceipt NoArgs
    )

deployScript :: DeployM (Record DeployResults)
deployScript = deployScriptWithGasSettings emptyGasSettings

deployScriptWithGasSettings :: GasSettings -> DeployM (Record DeployResults)
deployScriptWithGasSettings gasSettings = do
  superRareMarketAuctionV2 <-
    deployContractWithConfig
      { contractConfig: makeSuperRareMarketAuctionV2Config
      , gasSettings
      }
  pure { superRareMarketAuctionV2 }
