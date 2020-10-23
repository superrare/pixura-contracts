module Deploy.Contracts.SuperRareRoyaltyRegistry where

import Prelude
import Chanterelle.Internal.Deploy (DeployReceipt)
import Chanterelle.Internal.Types (ContractConfig, DeployM)
import Contracts.SuperRareRoyaltyRegistry as SuperRareRoyaltyRegistry
import Deploy.Utils (GasSettings, deployContractWithConfig)
import Migrations.Utils (emptyGasSettings)
import Network.Ethereum.Core.Signatures (Address)

type SuperRareRoyaltyRegistry
  = ( _iERC721TokenCreator :: Address )

makeSuperRareRoyaltyRegistryConfig :: Record SuperRareRoyaltyRegistry -> ContractConfig SuperRareRoyaltyRegistry
makeSuperRareRoyaltyRegistryConfig { _iERC721TokenCreator } =
  { filepath: "./contracts/build/SuperRareRoyaltyRegistry.json"
  , name: "SuperRareRoyaltyRegistry"
  , constructor: SuperRareRoyaltyRegistry.constructor
  , unvalidatedArgs: pure { _iERC721TokenCreator }
  }

type DeployResults
  = ( superRareRoyaltyRegistry :: DeployReceipt SuperRareRoyaltyRegistry
    )

deployScript ::
  Record SuperRareRoyaltyRegistry ->
  DeployM (Record DeployResults)
deployScript = deployScriptWithGasSettings emptyGasSettings

deployScriptWithGasSettings ::
  GasSettings ->
  Record SuperRareRoyaltyRegistry ->
  DeployM (Record DeployResults)
deployScriptWithGasSettings gasSettings args = do
  superRareRoyaltyRegistry <-
    deployContractWithConfig
      { contractConfig: makeSuperRareRoyaltyRegistryConfig args
      , gasSettings
      }
  pure { superRareRoyaltyRegistry }
