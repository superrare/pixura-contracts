module Deploy.Utils where

import Prelude
import Chanterelle.Deploy (deployContract)
import Chanterelle.Internal.Deploy (DeployReceipt)
import Chanterelle.Internal.Logging (LogLevel(..), log)
import Chanterelle.Internal.Types (ContractConfig, DeployConfig(DeployConfig), DeployM)
import Chanterelle.Internal.Utils (pollTransactionReceipt)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Reader.Class (ask)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Lens ((.~), (?~), (^.))
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error, error, throw)
import Foreign (ForeignError(..))
import Network.Ethereum.Core.BigNumber (decimal, divide, embed, parseBigNumber, pow, unsafeToInt)
import Network.Ethereum.Core.HexString (HexString, dropHex, toAscii)
import Network.Ethereum.Web3 (Address, BigNumber, CallError, ChainCursor(..), Provider, Transaction(..), TransactionOptions, TransactionReceipt(..), TransactionStatus(..), Web3, _data, _from, _gas, _gasPrice, _nonce, _to, _value, defaultTransactionOptions, runWeb3, unAddress)
import Network.Ethereum.Web3.Api (eth_call, eth_getBalance, eth_getTransaction, eth_getTransactionReceipt)
import Network.Ethereum.Web3.Types (NoPay)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Simple.JSON as JSON

throwOnCallError :: forall a m. MonadThrow Error m => m (Either CallError a) -> m a
throwOnCallError f =
  f
    >>= case _ of
        Left cerr -> throwError $ error $ show cerr
        Right x -> pure x

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

getFailedTxReason :: forall m. MonadAff m => HexString -> Provider -> m String
getFailedTxReason txHash provider = do
  eres <-
    liftAff
      $ runWeb3 provider do
          tx@(Transaction { gas, gasPrice, value, from, to, blockNumber, input, nonce }) <- eth_getTransaction txHash
          txReceipt <- eth_getTransactionReceipt txHash
          eth_call
            ( defaultTxOptions
                # _gas
                ?~ gas
                # _gasPrice
                ?~ gasPrice
                # _data
                ?~ input
                # _nonce
                ?~ nonce
                # _to
                .~ to
                # _from
                ?~ from
                # _value
                ?~ value
            )
            (maybe Latest BN blockNumber)
  r <- case eres of
    Left err -> liftEffect $ throw $ show err
    Right r -> pure r
  pure $ toAscii $ dropHex 136 r

awaitTxSuccess :: forall m. MonadAff m => HexString -> Provider -> m Unit
awaitTxSuccess txHash provider = do
  TransactionReceipt txReceipt <- pollTransactionReceipt txHash provider
  case txReceipt.status of
    Succeeded -> pure unit
    Failed -> do
      res <- getFailedTxReason txHash provider
      let
        txReason = case res of
          "" -> ""
          reason -> "Reason for failure: " <> res <> "\n"
      unsafeCrashWith $ "Transaction Failed w/ hash "
        <> show txHash
        <> "\n"
        <> txReason
        <> show txReceipt

awaitTxSuccessWeb3 :: HexString -> Web3 Unit
awaitTxSuccessWeb3 txHash = awaitTxSuccess txHash =<< ask

awaitTxSuccessAndLogEthStats :: HexString -> Web3 Unit
awaitTxSuccessAndLogEthStats txHash = do
  awaitTxSuccessWeb3 txHash
  logEthSpentOnTx txHash

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
  Transaction { gasPrice, from } <- eth_getTransaction txHash
  TransactionReceipt { gasUsed } <- eth_getTransactionReceipt txHash
  let
    weiSpent = gasPrice * gasUsed
  log Info $ "Eth spent on Tx:" <> show ((toNumber $ unsafeToInt (weiSpent `divide` pow (embed 10) 14)) / 10000.0)
  logBalanceAndPrint from

logBalanceAndPrint :: Address -> Web3 Unit
logBalanceAndPrint primAddr = do
  bal <- eth_getBalance primAddr Latest
  log Info
    $ "Current balance for address "
    <> show (unAddress primAddr)
    <> " is "
    <> show (toNumber (unsafeToInt (bal `divide` pow (embed 10) 14)) / 10000.0)
