module Deploy.Contracts.SuperRareLegacy where

import Prelude
import Chanterelle.Internal.Deploy (DeployReceipt)
import Chanterelle.Internal.Types (ContractConfig, DeployM)
import Contracts.V5.SuperRareLegacy as SuperRareLegacy
import Deploy.Utils (GasSettings, deployContractWithConfig)
import Migrations.Utils (emptyGasSettings)
import Network.Ethereum.Web3 (Address, UIntN)
import Network.Ethereum.Web3.Solidity.Sizes (S256)

type SuperRareLegacy
  = ( _lastTokenId :: UIntN S256, _name :: String, _symbol :: String, _oldSuperRare :: Address )

makeSuperRareLegacyConfig :: Record SuperRareLegacy -> ContractConfig SuperRareLegacy
makeSuperRareLegacyConfig { _name, _symbol, _oldSuperRare, _lastTokenId } =
  { filepath: "./contracts/v5/build/SuperRareLegacy.json"
  , name: "SuperRareLegacy"
  , constructor: SuperRareLegacy.constructor
  , unvalidatedArgs: pure { _lastTokenId, _name, _symbol, _oldSuperRare }
  }

type DeployResults
  = ( superRareLegacy :: DeployReceipt SuperRareLegacy
    )

deployScript :: Record SuperRareLegacy -> DeployM (Record DeployResults)
deployScript = deployScriptWithGasSettings emptyGasSettings

deployScriptWithGasSettings :: GasSettings -> Record SuperRareLegacy -> DeployM (Record DeployResults)
deployScriptWithGasSettings gasSettings srl = do
  superRareLegacy <-
    deployContractWithConfig
      { contractConfig: makeSuperRareLegacyConfig srl
      , gasSettings
      }
  pure { superRareLegacy }
