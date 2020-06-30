module Deploy.Contracts.SuperRareLegacy where

import Prelude
import Chanterelle.Internal.Deploy (DeployReceipt)
import Chanterelle.Internal.Logging (LogLevel(..), log)
import Chanterelle.Internal.Types (ContractConfig, DeployM)
import Contracts.V5.SuperRareLegacy as SuperRareLegacy
import Control.Monad.Error.Class (try)
import Data.Array (drop, take, (:))
import Data.Either (Either(..))
import Data.Lens ((?~))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for)
import Deploy.Utils (GasSettings, awaitTxSuccessWeb3, deployContractWithConfig, logBalanceAndPrint, logEthSpentOnTx, txOptsWithGasSettings)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Migrations.Utils (emptyGasSettings)
import Network.Ethereum.Core.BigNumber (BigNumber)
import Network.Ethereum.Web3 (Address, HexString, Provider, UIntN, _from, _to, runWeb3, unUIntN)
import Network.Ethereum.Web3.Solidity.Sizes (S256)

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
  = ( superRareLegacy :: DeployReceipt SuperRareLegacy )

type MintingDetails
  = { markedCompletedHash :: Maybe HexString
    , contractAddress :: Address
    , successfulTransactions ::
        Array
          { hash :: HexString
          , tokenIds :: Array BigNumber
          }
    , failedTransactions ::
        Array
          { hash :: HexString
          , error :: String
          , tokenIds :: Array BigNumber
          }
    }

emptyMintingDetails :: Address -> MintingDetails
emptyMintingDetails = { contractAddress: _, markedCompletedHash: Nothing, successfulTransactions: [], failedTransactions: [] }

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
  { primaryAccount :: Address
  , provider :: Provider
  , mintingDetails :: Maybe MintingDetails
  | r
  } ->
  GasSettings ->
  Array (UIntN S256) ->
  Address ->
  m MintingDetails
mintLegacyTokens { primaryAccount, provider, mintingDetails: mmd } gasSettings tokenIds addr = do
  let
    md = fromMaybe (emptyMintingDetails addr) mmd
  if (addr /= md.contractAddress) then
    liftEffect $ throw
      $ "Minting address, "
      <> show addr
      <> ", does not match minting details address, "
      <> show md.contractAddress
  else
    pure unit
  avMd <- liftAff $ AVar.new md
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
                txHash <- attempt 3 $ SuperRareLegacy.mintLegacyTokens txOpts { _tokenIds }
                log Info $ "Hash is: " <> show txHash
                res <- try $ awaitTxSuccessWeb3 txHash
                case res of
                  Left err -> do
                    log Error $ "Caught error on hash: `"
                      <> show txHash
                      <> "`.\n"
                      <> show err
                    updateFailed txHash (show err) _tokenIds avMd
                  Right _ -> do
                    updateSuccess txHash _tokenIds avMd
                    logEthSpentOnTx txHash
          md' <- liftAff $ AVar.read avMd
          case md'.markedCompletedHash of
            Nothing -> do
              txHash <- SuperRareLegacy.markMintingCompleted txOpts
              awaitTxSuccessWeb3 txHash
              updateComplete txHash avMd
            _ -> pure unit
          log Info $ "Minting completed and marked in smart contract"
  md' <- liftAff $ AVar.take avMd
  case res of
    Left err -> log Error (show err)
    Right _ -> pure unit
  pure md'
  where
  attempt n f = do
    res <- try f
    case res of
      Left err ->
        if (n - 1) == 0 then
          liftEffect $ throw $ show err
        else do
          liftAff $ delay (Milliseconds 3000.0)
          log Warn $ "Errored with " <> show (n - 1) <> "attempts left.\n" <> show err
          attempt (n - 1) f
      Right v -> pure v

  updateComplete hash avMd =
    liftAff do
      md <- AVar.take avMd
      AVar.put md { markedCompletedHash = Just hash } avMd

  updateFailed hash err tids avMd =
    liftAff do
      md <- AVar.take avMd
      AVar.put
        ( md
            { failedTransactions =
              md.failedTransactions
                <> [ { hash, tokenIds: tids <#> unUIntN, error: err } ]
            }
        )
        avMd

  updateSuccess hash tids avMd =
    liftAff do
      md <- AVar.take avMd
      AVar.put
        ( md
            { successfulTransactions =
              md.successfulTransactions
                <> [ { hash, tokenIds: tids <#> unUIntN } ]
            }
        )
        avMd

  chunk n [] = []

  chunk n xs = take n xs : chunk n (drop n xs)
