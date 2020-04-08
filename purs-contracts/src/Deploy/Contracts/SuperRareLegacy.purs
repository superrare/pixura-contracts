module Deploy.Contracts.SuperRareLegacy where

import Prelude
import Chanterelle.Internal.Deploy (DeployReceipt)
import Chanterelle.Internal.Logging (LogLevel(..), log)
import Chanterelle.Internal.Types (ContractConfig, DeployConfig(..), DeployM)
import Contracts.V5.SuperRareLegacy as SuperRareLegacy
import Control.Monad.Reader (ask)
import Data.Array (drop, take, (:))
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Lens ((?~))
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Deploy.Utils (GasSettings(..), awaitTxSuccess, awaitTxSuccessWeb3, deployContractWithConfig, txOptsWithGasSettings)
import Effect.Aff.Class (class MonadAff, liftAff)
import Migrations.Utils (emptyGasSettings)
import Network.Ethereum.Core.BigNumber (decimal, divide, embed, parseBigNumber, pow, unsafeToInt)
import Network.Ethereum.Web3 (Address, ChainCursor(..), Ether, Provider, Transaction(..), TransactionReceipt(..), UIntN, Value, _from, _to, formatValue, mkValue, runWeb3, toMinorUnit, unAddress, unUIntN)
import Network.Ethereum.Web3.Api (eth_estimateGas, eth_getBalance, eth_getTransaction, eth_getTransactionReceipt)
import Network.Ethereum.Web3.Solidity.Sizes (S256, s256)

type SuperRareLegacy
  = ( _name :: String
    , _symbol :: String
    , _oldSuperRare :: Address
    )

makeSuperRareLegacyConfig :: Record SuperRareLegacy -> ContractConfig SuperRareLegacy
makeSuperRareLegacyConfig { _name, _symbol, _oldSuperRare } =
  { filepath: "./contracts/v5/build/SuperRareLegacy.json"
  , name: "SuperRareLegacy"
  , constructor: SuperRareLegacy.constructor
  , unvalidatedArgs: pure { _name, _symbol, _oldSuperRare }
  }

type DeployResults
  = ( superRareLegacy :: DeployReceipt SuperRareLegacy
    )

deployScript ::
  Record SuperRareLegacy ->
  DeployM (Record DeployResults)
deployScript = deployScriptWithGasSettings emptyGasSettings

deployScriptWithGasSettings ::
  GasSettings ->
  Record SuperRareLegacy -> DeployM (Record DeployResults)
deployScriptWithGasSettings gasSettings srl = do
  superRareLegacy@{ deployAddress } <-
    deployContractWithConfig
      { contractConfig: makeSuperRareLegacyConfig srl
      , gasSettings
      }
  pure { superRareLegacy }

mintLegacyTokens ::
  forall m r.
  MonadAff m =>
  { primaryAccount :: Address, provider :: Provider | r } ->
  GasSettings ->
  Array (UIntN S256) ->
  Address ->
  m Unit
mintLegacyTokens { primaryAccount, provider } gasSettings tokenIds addr = do
  res <-
    liftAff
      $ runWeb3 provider do
          let
            txOpts =
              txOptsWithGasSettings gasSettings
                # _from
                ?~ primaryAccount
                # _to
                ?~ addr
          void
            $ for (chunk 20 tokenIds) \_tokenIds -> do
                logBalanceAndPrint primaryAccount
                log Info $ "Minting Legacy tokens for:\n" <> show _tokenIds
                txHash <- SuperRareLegacy.mintLegacyTokens txOpts { _tokenIds }
                log Info $ "Hash is: " <> show txHash
                awaitTxSuccessWeb3 txHash
                logEthSpentOnTx txHash
          txHash <- SuperRareLegacy.markMintingCompleted txOpts
          awaitTxSuccessWeb3 txHash
          log Info $ "Minting completed and marked in smart contract"
  case res of
    Left err -> log Error (show err)
    Right _ -> pure unit
  where
  chunk n [] = []

  chunk n xs = take n xs : chunk n (drop n xs)

  logEthSpentOnTx txHash = do
    Transaction { gasPrice } <- eth_getTransaction txHash
    TransactionReceipt { gasUsed } <- eth_getTransactionReceipt txHash
    let
      weiSpent = gasPrice * gasUsed
    log Info $ "Eth spent on Tx:" <> show ((toNumber $ unsafeToInt (weiSpent `divide` pow (embed 10) 14)) / 10000.0)

  logBalanceAndPrint primAddr = do
    bal <- eth_getBalance primAddr Latest
    log Info
      $ "Current balance for address "
      <> show (unAddress primAddr)
      <> " is "
      <> show (toNumber (unsafeToInt (bal `divide` pow (embed 10) 14)) / 10000.0)
