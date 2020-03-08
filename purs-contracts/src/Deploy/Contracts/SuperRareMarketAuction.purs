module Deploy.Contracts.SuperRareMarketAuction where

import Prelude

import Chanterelle.Deploy (deployContract)
import Chanterelle.Internal.Deploy (DeployReceipt)
import Chanterelle.Internal.Types (DeployM, DeployConfig(..), ContractConfig, NoArgs, noArgs, constructorNoArgs)
import Control.Monad.Reader.Class (ask)
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Network.Ethereum.Core.BigNumber (decimal, parseBigNumber)
import Network.Ethereum.Web3 (_from, _gas, _gasPrice, defaultTransactionOptions)
import Partial.Unsafe (unsafePartial)


makeSuperRareMarketAuctionConfig :: ContractConfig NoArgs
makeSuperRareMarketAuctionConfig =
  { filepath: "./contracts/build/contracts/SuperRareMarketAuction.json"
  , name: "SuperRareMarketAuction"
  , constructor: constructorNoArgs
  , unvalidatedArgs: noArgs
  }

type DeployResults
  = ( superRareMarketAuction :: DeployReceipt NoArgs
    )

deployScript :: DeployM (Record DeployResults)
deployScript = do
  deployCfg@(DeployConfig { primaryAccount, provider }) <- ask
  let
    bigGasLimit = unsafePartial fromJust $ parseBigNumber decimal "6712388"

    bigGasPrice = unsafePartial fromJust $ parseBigNumber decimal "10000000000"

    txOpts =
      defaultTransactionOptions # _from ?~ primaryAccount
        # _gas
        ?~ bigGasLimit
        # _gasPrice
        ?~ bigGasPrice
  superRareMarketAuction <- deployContract txOpts $ makeSuperRareMarketAuctionConfig
  pure { superRareMarketAuction }
