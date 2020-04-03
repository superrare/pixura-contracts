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
import Effect.Aff.Class (liftAff)
import Migrations.Utils (emptyGasSettings)
import Network.Ethereum.Core.BigNumber (decimal, divide, embed, parseBigNumber, pow, unsafeToInt)
import Network.Ethereum.Web3 (Address, ChainCursor(..), Ether, Transaction(..), TransactionReceipt(..), UIntN, Value, _from, _to, formatValue, mkValue, runWeb3, toMinorUnit, unAddress, unUIntN)
import Network.Ethereum.Web3.Api (eth_getBalance, eth_getTransaction, eth_getTransactionReceipt)
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
  Array (UIntN S256) ->
  Record SuperRareLegacy ->
  DeployM (Record DeployResults)
deployScript =
  deployScriptWithGasSettings
    ( GasSettings
        { gasLimit: parseBigNumber decimal "107123880"
        , gasPrice: parseBigNumber decimal "5000000000"
        }
    )

deployScriptWithGasSettings ::
  GasSettings ->
  Array (UIntN S256) ->
  Record SuperRareLegacy -> DeployM (Record DeployResults)
deployScriptWithGasSettings gasSettings tokenIds srl = do
  superRareLegacy@{ deployAddress } <-
    deployContractWithConfig
      { contractConfig: makeSuperRareLegacyConfig srl
      , gasSettings
      }
  mintLegacyTokens gasSettings tokenIds deployAddress
  pure { superRareLegacy }

mintLegacyTokens :: GasSettings -> Array (UIntN S256) -> Address -> DeployM Unit
mintLegacyTokens gasSettings tokenIds addr = do
  DeployConfig { primaryAccount, provider } <- ask
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
            $ for (chunk 500 tokenIds) \_tokenIds -> do
                logBalanceAndPrint primaryAccount
                log Info $ "Minting Legacy tokens for:\n" <> show _tokenIds
                txHash <- SuperRareLegacy.mintLegacyTokens txOpts { _tokenIds }
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
