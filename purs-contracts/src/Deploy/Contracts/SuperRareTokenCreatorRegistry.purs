module Deploy.Contracts.SuperRareTokenCreatorRegistry where

import Prelude
import Chanterelle.Internal.Deploy (DeployReceipt)
import Chanterelle.Internal.Types (ContractConfig, DeployM)
import Contracts.SuperRareTokenCreatorRegistry as SuperRareTokenCreatorRegistry
import Deploy.Utils (GasSettings, deployContractWithConfig)
import Migrations.Utils (emptyGasSettings)
import Network.Ethereum.Core.Signatures (Address)

type SuperRareTokenCreatorRegistry
  = ( _iERC721Creators :: (Array Address) )

makeSuperRareTokenCreatorRegistryConfig :: Record SuperRareTokenCreatorRegistry -> ContractConfig SuperRareTokenCreatorRegistry
makeSuperRareTokenCreatorRegistryConfig { _iERC721Creators } =
  { filepath: "./contracts/build/SuperRareTokenCreatorRegistry.json"
  , name: "SuperRareTokenCreatorRegistry"
  , constructor: SuperRareTokenCreatorRegistry.constructor
  , unvalidatedArgs: pure { _iERC721Creators }
  }

type DeployResults
  = ( superRareTokenCreatorRegistry :: DeployReceipt SuperRareTokenCreatorRegistry
    )

deployScript ::
  Record SuperRareTokenCreatorRegistry ->
  DeployM (Record DeployResults)
deployScript = deployScriptWithGasSettings emptyGasSettings

deployScriptWithGasSettings ::
  GasSettings ->
  Record SuperRareTokenCreatorRegistry ->
  DeployM (Record DeployResults)
deployScriptWithGasSettings gasSettings args = do
  superRareTokenCreatorRegistry <-
    deployContractWithConfig
      { contractConfig: makeSuperRareTokenCreatorRegistryConfig args
      , gasSettings
      }
  pure { superRareTokenCreatorRegistry }
