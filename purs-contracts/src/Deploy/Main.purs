module Deploy.Main where

import Prelude
import Chanterelle.Deploy (deployContract)
import Chanterelle.Internal.Deploy (DeployReceipt)
import Chanterelle.Internal.Types (DeployM, DeployConfig(..), ContractConfig, NoArgs, noArgs, constructorNoArgs)
import Contracts.SuperRareV2 as SuperRareV2
import Control.Monad.Reader.Class (ask)
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Network.Ethereum.Core.BigNumber (decimal, parseBigNumber)
import Network.Ethereum.Web3 (Address, _from, _gas, _gasPrice, defaultTransactionOptions)
import Partial.Unsafe (unsafePartial)

deploy :: DeployM Unit
deploy = void deployScript

makeSuperRareMarketAuctionV2Config :: ContractConfig NoArgs
makeSuperRareMarketAuctionV2Config =
  { filepath: "./contracts/build/contracts/SuperRareMarketAuctionV2.json"
  , name: "SuperRareMarketAuctionV2"
  , constructor: constructorNoArgs
  , unvalidatedArgs: noArgs
  }

type SuperRareV2
  = ( _name :: String, _symbol :: String, _oldSuperRare :: Address )

makeSuperRareV2Config :: Record SuperRareV2 -> ContractConfig SuperRareV2
makeSuperRareV2Config { _name, _symbol, _oldSuperRare } =
  { filepath: "./contracts/build/contracts/SuperRareV2.json"
  , name: "SuperRareV2"
  , constructor: SuperRareV2.constructor
  , unvalidatedArgs: pure { _name, _symbol, _oldSuperRare }
  }

type DeployResults
  = ( superRareMarketAuctionV2 :: DeployReceipt NoArgs
    )

-- web3 connection
-- npm run build this
-- then chanterelle deploy ./output/blah/index.js
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
  superRareMarketAuctionV2 <- deployContract txOpts makeSuperRareMarketAuctionV2Config
  pure { superRareMarketAuctionV2 }
