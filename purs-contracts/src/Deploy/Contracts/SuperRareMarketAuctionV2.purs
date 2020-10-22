module Deploy.Contracts.SuperRareMarketAuctionV2 where

import Prelude
import Chanterelle.Internal.Deploy (DeployReceipt)
import Chanterelle.Internal.Types (ContractConfig, DeployM)
import Contracts.SuperRareMarketAuctionV2 as SuperRareMarketAuctionV2
import Deploy.Utils (GasSettings, deployContractWithConfig)
import Migrations.Utils (emptyGasSettings)
import Network.Ethereum.Core.Signatures (Address)

type SuperRareMarketAuctionV2
  = ( _iERC721CreatorRoyalty :: Address, _iMarketSettings :: Address
    )

makeSuperRareMarketAuctionV2Config :: Record SuperRareMarketAuctionV2 -> ContractConfig SuperRareMarketAuctionV2
makeSuperRareMarketAuctionV2Config { _iERC721CreatorRoyalty, _iMarketSettings } =
  { filepath: "./contracts/build/SuperRareMarketAuctionV2.json"
  , name: "SuperRareMarketAuctionV2"
  , constructor: SuperRareMarketAuctionV2.constructor
  , unvalidatedArgs: pure { _iERC721CreatorRoyalty, _iMarketSettings }
  }

type DeployResults
  = ( superRareMarketAuctionV2 :: DeployReceipt SuperRareMarketAuctionV2
    )

deployScript ::
  Record SuperRareMarketAuctionV2 -> DeployM (Record DeployResults)
deployScript = deployScriptWithGasSettings emptyGasSettings

deployScriptWithGasSettings ::
  GasSettings ->
  Record SuperRareMarketAuctionV2 ->
  DeployM (Record DeployResults)
deployScriptWithGasSettings gasSettings cArgs = do
  superRareMarketAuctionV2 <-
    deployContractWithConfig
      { contractConfig: makeSuperRareMarketAuctionV2Config cArgs
      , gasSettings
      }
  pure { superRareMarketAuctionV2 }
