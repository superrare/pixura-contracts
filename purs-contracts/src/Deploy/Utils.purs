module Deploy.Utils where

import Prelude
import Chanterelle.Internal.Types (ContractConfig, DeployM(..))
import Chanterelle.Internal.Utils (pollTransactionReceipt)
import Control.Monad.Reader (ask)
import Data.Lens ((?~))
import Data.Maybe (Maybe, fromJust, fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Network.Ethereum.Core.BigNumber (decimal, parseBigNumber)
import Network.Ethereum.Core.HexString (HexString)
import Network.Ethereum.Web3 (BigNumber, Provider, TransactionOptions(..), TransactionReceipt(..), TransactionStatus(..), Web3, _gas, _gasPrice, defaultTransactionOptions)
import Network.Ethereum.Web3.Types (NoPay)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Chanterelle.Deploy (deployContract)
import Chanterelle.Internal.Deploy (DeployReceipt)
import Chanterelle.Internal.Types (DeployM, DeployConfig(..), ContractConfig, NoArgs, noArgs, constructorNoArgs)
import Control.Monad.Reader.Class (ask)
import Data.Lens ((.~), (?~), (^.), (^?))
import Data.Maybe (Maybe(..), fromJust, fromMaybe, maybe)
import Network.Ethereum.Core.BigNumber (BigNumber, decimal, parseBigNumber)
import Network.Ethereum.Web3 (_from, _gas, _gasPrice, defaultTransactionOptions)
import Partial.Unsafe (unsafePartial)

defaultTxOptions :: TransactionOptions NoPay
defaultTxOptions =
  let
    defaultGasLimit = unsafePartial fromJust $ parseBigNumber decimal "67123880"

    defaultGasPrice = unsafePartial fromJust $ parseBigNumber decimal "10000000000"
  in
    defaultTransactionOptions
      # _gas
      ?~ defaultGasLimit
      # _gasPrice
      ?~ defaultGasPrice

txOptsWithGasSettings ::
  GasSettings ->
  TransactionOptions NoPay
txOptsWithGasSettings { gasLimit, gasPrice } =
  defaultTxOptions # _gas .~ maybe (defaultTxOptions ^. _gas) Just gasLimit
    # _gasPrice
    .~ maybe (defaultTxOptions ^. _gasPrice) Just gasPrice

awaitTxSuccess :: forall m. MonadAff m => HexString -> Provider -> m Unit
awaitTxSuccess txHash provider = do
  TransactionReceipt txReceipt <- pollTransactionReceipt txHash provider
  case txReceipt.status of
    Succeeded -> pure unit
    Failed -> unsafeCrashWith $ "Transaction Failed w/ hash " <> show txHash <> "\n" <> show txReceipt

awaitTxSuccessWeb3 :: HexString -> Web3 Unit
awaitTxSuccessWeb3 txHash = awaitTxSuccess txHash =<< ask

deployContractWithConfig ::
  forall a.
  { gasSettings :: GasSettings
  , contractConfig :: ContractConfig a
  } ->
  DeployM (DeployReceipt a)
deployContractWithConfig { contractConfig, gasSettings } = do
  deployCfg@(DeployConfig { primaryAccount, provider }) <- ask
  deployContract (txOptsWithGasSettings gasSettings) contractConfig

type GasSettings
  = { gasLimit :: Maybe BigNumber
    , gasPrice :: Maybe BigNumber
    }
