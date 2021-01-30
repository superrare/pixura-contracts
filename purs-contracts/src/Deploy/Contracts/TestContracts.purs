module Deploy.Contracts.TestContracts where

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

-----------------------------------------------------------------------------
-- | deploy
-----------------------------------------------------------------------------
type DeployResults
  = ( testAssertFailOnPay :: DeployReceipt NoArgs
    , testExpensiveWallet :: DeployReceipt NoArgs
    , testRequireFailOnPay :: DeployReceipt NoArgs
    , testRevertOnPay :: DeployReceipt NoArgs
    )

deployScript :: DeployM (Record DeployResults)
deployScript = do
  deployCfg@(DeployConfig { primaryAccount, provider }) <- ask
  let
    bigGasLimit = unsafePartial fromJust $ parseBigNumber decimal "67123880"

    bigGasPrice = unsafePartial fromJust $ parseBigNumber decimal "10000000000"

    txOpts =
      defaultTransactionOptions # _from ?~ primaryAccount
        # _gas
        ?~ bigGasLimit
        # _gasPrice
        ?~ bigGasPrice
  testExpensiveWallet <- deployContract txOpts makeTestExpensiveWalletConfig
  testAssertFailOnPay <- deployContract txOpts makeTestAssertFailOnPayConfig
  testRequireFailOnPay <- deployContract txOpts makeTestRequireFailOnPayConfig
  testRevertOnPay <- deployContract txOpts makeTestRevertOnPayConfig
  pure
    { testAssertFailOnPay
    , testExpensiveWallet
    , testRequireFailOnPay
    , testRevertOnPay
    }

-----------------------------------------------------------------------------
-- | configs
-----------------------------------------------------------------------------
makeTestAssertFailOnPayConfig :: ContractConfig NoArgs
makeTestAssertFailOnPayConfig =
  { filepath: "./contracts/build/TestAssertFailOnPay.json"
  , name: "TestAssertFailOnPay"
  , constructor: constructorNoArgs
  , unvalidatedArgs: noArgs
  }

makeTestExpensiveWalletConfig :: ContractConfig NoArgs
makeTestExpensiveWalletConfig =
  { filepath: "./contracts/build/TestExpensiveWallet.json"
  , name: "TestExpensiveWallet"
  , constructor: constructorNoArgs
  , unvalidatedArgs: noArgs
  }

makeTestRequireFailOnPayConfig :: ContractConfig NoArgs
makeTestRequireFailOnPayConfig =
  { filepath: "./contracts/build/TestRequireFailOnPay.json"
  , name: "TestRequireFailOnPay"
  , constructor: constructorNoArgs
  , unvalidatedArgs: noArgs
  }

makeTestRevertOnPayConfig :: ContractConfig NoArgs
makeTestRevertOnPayConfig =
  { filepath: "./contracts/build/TestRevertOnPay.json"
  , name: "TestRevertOnPay"
  , constructor: constructorNoArgs
  , unvalidatedArgs: noArgs
  }
