module Deploy.Contracts.SuperRareAuctionHouse where

import Prelude
import Chanterelle.Internal.Deploy (DeployReceipt)
import Chanterelle.Internal.Types (ContractConfig, DeployM)
import Contracts.SuperRareAuctionHouse as SuperRareAuctionHouse
import Deploy.Utils (GasSettings, deployContractWithConfig)
import Migrations.Utils (emptyGasSettings)
import Network.Ethereum.Core.Signatures (Address)

type SuperRareAuctionHouse
  = ( _iERC721CreatorRoyalty :: Address, _iMarketSettings :: Address
    )

makeSuperRareAuctionHouseConfig :: Record SuperRareAuctionHouse -> ContractConfig SuperRareAuctionHouse
makeSuperRareAuctionHouseConfig { _iERC721CreatorRoyalty, _iMarketSettings } =
  { filepath: "./contracts/build/SuperRareAuctionHouse.json"
  , name: "SuperRareAuctionHouse"
  , constructor: SuperRareAuctionHouse.constructor
  , unvalidatedArgs: pure { _iERC721CreatorRoyalty, _iMarketSettings }
  }

type DeployResults
  = ( superRareAuctionHouse :: DeployReceipt SuperRareAuctionHouse
    )

deployScript ::
  Record SuperRareAuctionHouse -> DeployM (Record DeployResults)
deployScript = deployScriptWithGasSettings emptyGasSettings

deployScriptWithGasSettings ::
  GasSettings ->
  Record SuperRareAuctionHouse ->
  DeployM (Record DeployResults)
deployScriptWithGasSettings gasSettings cArgs = do
  superRareAuctionHouse <-
    deployContractWithConfig
      { contractConfig: makeSuperRareAuctionHouseConfig cArgs
      , gasSettings
      }
  pure { superRareAuctionHouse }
