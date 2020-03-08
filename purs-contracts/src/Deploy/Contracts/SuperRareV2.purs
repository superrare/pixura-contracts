module Deploy.Contracts.SuperRareV2 where

import Prelude

import Chanterelle.Deploy (deployContract)
import Chanterelle.Internal.Deploy (DeployReceipt)
import Chanterelle.Internal.Types (ContractConfig, DeployConfig(..), DeployM)
import Contracts.SuperRareV2 as SuperRareV2
import Control.Monad.Reader.Class (ask)
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Network.Ethereum.Core.BigNumber (decimal, parseBigNumber)
import Network.Ethereum.Web3 (Address, _from, _gas, _gasPrice, defaultTransactionOptions)
import Partial.Unsafe (unsafePartial)

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
  = ( superRareV2 :: DeployReceipt SuperRareV2
    )

deployScript :: Record SuperRareV2 -> DeployM (Record DeployResults)
deployScript srv2 = do
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
  superRareV2 <- deployContract txOpts $ makeSuperRareV2Config srv2
  pure { superRareV2 }
