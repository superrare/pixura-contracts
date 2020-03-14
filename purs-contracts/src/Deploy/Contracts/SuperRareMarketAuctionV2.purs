module Deploy.Contracts.SuperRareMarketAuctionV2 where

import Prelude
import Chanterelle.Deploy (deployContract)
import Chanterelle.Internal.Deploy (DeployReceipt)
import Chanterelle.Internal.Types (DeployM, DeployConfig(..), ContractConfig, NoArgs, noArgs, constructorNoArgs)
import Control.Monad.Reader.Class (ask)
import Data.Lens ((.~), (?~), (^.), (^?))
import Data.Maybe (Maybe(..), fromJust, fromMaybe, maybe)
import Deploy.Utils (defaultTxOptions, deployContractWithConfig)
import Network.Ethereum.Core.BigNumber (BigNumber, decimal, parseBigNumber)
import Network.Ethereum.Web3 (_from, _gas, _gasPrice, defaultTransactionOptions)
import Partial.Unsafe (unsafePartial)

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

deployStrict :: DeployM (Record DeployResults)
deployStrict = deployScriptWithGasSettings { gasLimit: Nothing, gasPrice: Nothing }

deployScriptWithGasSettings ::
  { gasSettings {gasLimit :: Maybe BigNumber, gasPrice :: Maybe BigNumber} } -> DeployM (Record DeployResults)
deployScriptWithGasSettings { gasLimit, gasPrice } = do
  superRareMarketAuctionV2 <-
    deployContractWithConfig
      { contractConfig: makeSuperRareMarketAuctionV2Config
      , gasLimit
      , gasPrice
      }
  pure { superRareMarketAuctionV2 }
