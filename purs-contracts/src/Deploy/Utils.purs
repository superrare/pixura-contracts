module Deploy.Utils where

import Prelude
import Chanterelle.Deploy (deployContract)
import Chanterelle.Internal.Deploy (DeployReceipt)
import Chanterelle.Internal.Logging (LogLevel(..), log)
import Chanterelle.Internal.Types (ContractConfig, DeployConfig(DeployConfig), DeployM)
import Chanterelle.Internal.Utils (pollTransactionReceipt)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader.Class (ask)
import Data.Int (toNumber)
import Data.Lens ((.~), (?~), (^.))
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Effect.Aff.Class (class MonadAff)
import Foreign (ForeignError(..))
import Network.Ethereum.Core.BigNumber (decimal, divide, embed, parseBigNumber, pow, unsafeToInt)
import Network.Ethereum.Core.HexString (HexString)
import Network.Ethereum.Web3 (Address, BigNumber, ChainCursor(..), Provider, Transaction(..), TransactionOptions, TransactionReceipt(..), TransactionStatus(..), Web3, _from, _gas, _gasPrice, defaultTransactionOptions, unAddress)
import Network.Ethereum.Web3.Api (eth_getBalance, eth_getTransaction, eth_getTransactionReceipt)
import Network.Ethereum.Web3.Types (NoPay)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Simple.JSON as JSON

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
txOptsWithGasSettings (GasSettings { gasLimit, gasPrice }) =
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
  deployContract (txOptsWithGasSettings gasSettings # _from ?~ primaryAccount) contractConfig

newtype GasSettings
  = GasSettings
  { gasLimit :: Maybe BigNumber
  , gasPrice :: Maybe BigNumber
  }

derive newtype instance gasSettingsEq :: Eq GasSettings

derive newtype instance gasSettingsShow :: Show GasSettings

instance gasSettingsReadForeign :: JSON.ReadForeign GasSettings where
  readImpl f = do
    ({ gasLimit, gasPrice } :: { gasLimit :: Maybe String, gasPrice :: Maybe String }) <- JSON.readImpl f
    gl <- maybe (pure Nothing) (map Just <<< parseDecimalBigNum "gasLimit") gasLimit
    gp <- maybe (pure Nothing) (map Just <<< parseDecimalBigNum "gasPrice") gasPrice
    pure $ GasSettings { gasLimit: gl, gasPrice: gp }
    where
    parseDecimalBigNum name val =
      maybe
        ( throwError $ singleton $ ForeignError
            $ "Failed to parse "
            <> name
            <> ": "
            <> show val
            <> " as a decimal BigNumber."
        )
        pure
        $ parseBigNumber decimal val

logEthSpentOnTx :: HexString -> Web3 Unit
logEthSpentOnTx txHash = do
  Transaction { gasPrice } <- eth_getTransaction txHash
  TransactionReceipt { gasUsed } <- eth_getTransactionReceipt txHash
  let
    weiSpent = gasPrice * gasUsed
  log Info $ "Eth spent on Tx:" <> show ((toNumber $ unsafeToInt (weiSpent `divide` pow (embed 10) 14)) / 10000.0)

logBalanceAndPrint :: Address -> Web3 Unit
logBalanceAndPrint primAddr = do
  bal <- eth_getBalance primAddr Latest
  log Info
    $ "Current balance for address "
    <> show (unAddress primAddr)
    <> " is "
    <> show (toNumber (unsafeToInt (bal `divide` pow (embed 10) 14)) / 10000.0)
