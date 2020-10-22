module Migrations.SuperRareAuctionHouse where

import Prelude
import Chanterelle.Internal.Logging (LogLevel(..), log)
import Chanterelle.Internal.Types (DeployConfig(..), throwDeploy)
import Contracts.Marketplace.MarketplaceSettings as MarketplaceSettings
import Contracts.SuperRareMarketAuctionV2 as SuperRareMarketAuctionV2
import Control.Monad.Reader (ask)
import Data.Array (catMaybes, concat, drop, elem, filter, nub, take, (:))
import Data.Either (Either(..), either)
import Data.Lens ((?~))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for, sequence)
import Deploy.Contracts.SuperRareMarketAuctionV2 (deployScriptWithGasSettings)
import Deploy.Utils (GasSettings, awaitTxSuccessWeb3, throwOnCallError, txOptsWithGasSettings)
import Effect (Effect)
import Effect.Aff (Aff, error, runAff_, throwError, try)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (throw, throwException)
import Migrations.Utils (attempt, emptyGasSettings, runMigration)
import Network.Ethereum.Core.HexString (HexString, nullWord, toAscii)
import Network.Ethereum.Web3 (Address, BigNumber, ChainCursor(..), Provider, TransactionOptions, UIntN, _from, _to, embed, runWeb3, uIntNFromBigNumber, unAddress, unUIntN)
import Network.Ethereum.Web3.Solidity.Sizes (S256, s256)
import Network.Ethereum.Web3.Types (NoPay)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Process (cwd)
import Simple.Graphql.Query (runQuery)
import Simple.Graphql.Types (GraphQlQuery(..), runQueryT)
import Simple.JSON (readJSON, writeJSON)

type MigrationArgs
  = { v2SuperRareAddress :: Address
    , oldSuperRareAddress :: Address
    , legacySuperRareAddress :: Address
    , pixuraApi :: { url :: String, apiKey :: String }
    , migrationDetailsFile :: Maybe String
    }

type MigrationDetails
  = { marketContractAddress :: Address
    , successfulTransactions ::
        Array
          { hash :: HexString
          , tokens ::
              Array
                { tokenId :: BigNumber
                , contractAddress :: Address
                }
          }
    , failedTransactions ::
        Array
          { hash :: HexString
          , error :: String
          , tokens ::
              Array
                { tokenId :: BigNumber
                , contractAddress :: Address
                }
          }
    }

emptyMigrationDetails :: Address -> MigrationDetails
emptyMigrationDetails =
  { marketContractAddress: _
  , successfulTransactions: []
  , failedTransactions: []
  }

main :: Effect Unit
main =
  runMigration \(args :: { gasSettings :: Maybe GasSettings, migrationArgs :: MigrationArgs }) -> do
    DeployConfig { provider, primaryAccount } <- ask
    cwd' <- liftEffect cwd
    let
      { migrationArgs
      , gasSettings: mgs
      } = args

      gasSettings = fromMaybe emptyGasSettings mgs

      { migrationArgs
      , gasSettings: mgs
      } = args

      { v2SuperRareAddress
      , oldSuperRareAddress
      , legacySuperRareAddress
      , migrationDetailsFile
      , pixuraApi: { url, apiKey }
      } = migrationArgs

      txOpts = txOptsWithGasSettings gasSettings # _from ?~ primaryAccount

      writeFilename = fromMaybe (cwd' <> "/migration-details.json") migrationDetailsFile

      readJSONFile n = do
        t <- liftAff $ FS.readTextFile UTF8 n
        case readJSON t of
          Left err -> throwDeploy (error $ show err)
          Right v -> pure v
    migrationDetails <- case migrationDetailsFile of
      Nothing -> do
        { superRareMarketAuctionV2: { deployAddress } } <- deployScriptWithGasSettings gasSettings
        pure $ emptyMigrationDetails deployAddress
      Just mdf -> readJSONFile mdf
    avMd <- liftAff $ AVar.new migrationDetails
    let
      batchMark =
        batchMarkSold
          { url, apiKey, txOpts, migrationDetails: avMd, writeFilename, provider }
          100
          migrationDetails.marketContractAddress

      markSolds = [ batchMark v2SuperRareAddress v2SuperRareAddress, batchMark oldSuperRareAddress legacySuperRareAddress ]
    eres <- liftAff $ try $ sequence markSolds
    mmd <- liftAff $ AVar.tryRead avMd
    case mmd of
      Nothing -> throwDeploy (error "MigrationDetails found to be empty")
      Just md -> liftAff $ FS.writeTextFile UTF8 writeFilename (writeJSON md)
    case eres of
      Left err -> do
        throwDeploy (error $ show err)
      _ -> pure unit
    pure unit

-----------------------------------------------------------------------------
-- | batchMarkSold
-----------------------------------------------------------------------------
batchMarkSold ::
  { url :: String
  , apiKey :: String
  , writeFilename :: String
  , migrationDetails :: AVar.AVar MigrationDetails
  , txOpts :: TransactionOptions NoPay
  , provider :: Provider
  } ->
  Int ->
  Address ->
  Address ->
  Address ->
  Aff Unit
batchMarkSold cfg batchSize deployAddress _originContract upgradedOriginContract = do
  let
    { url, apiKey, txOpts, migrationDetails, provider, writeFilename } = cfg
  log Debug $ "Looking up sold tokens for contract: " <> addressToString _originContract
  soldTokens <- lookUpSoldTokens { tokenContract: _originContract, url, apiKey }
  liftAff do
    eres <-
      runWeb3 provider $ void $ batch batchSize soldTokens markTokens
    either (throwError <<< error <<< show) pure eres
  where
  handleFailure hash err tids md = do
    log Error $ "Caught error batching tokens as sold:\n" <> show err
    let
      failed = { hash, error: err, tokens: map (tokenAndContract upgradedOriginContract) tids }

      md' = md { failedTransactions = md.failedTransactions <> [ failed ] }
    liftAff $ AVar.put md' cfg.migrationDetails

  handleSuccess hash tids md = do
    let
      success = { hash, tokens: map (tokenAndContract upgradedOriginContract) tids }

      md' = md { successfulTransactions = md.successfulTransactions <> [ success ] }
    liftAff $ AVar.put md' cfg.migrationDetails

  markTokens _tokenIds = do
    let
      { url, apiKey, txOpts, migrationDetails, provider, writeFilename } = cfg
    md <- liftAff $ AVar.take migrationDetails
    eres <-
      try do
        let
          tokenAndAddrs = concat $ md.successfulTransactions <#> \{ tokens } -> tokens

          filteredTokens = filter (\tid -> not $ elem { tokenId: unUIntN tid, contractAddress: upgradedOriginContract } tokenAndAddrs) _tokenIds
        ims <-
          throwOnCallError
            $ SuperRareMarketAuctionV2.iMarketplaceSettings
                (txOpts # _to ?~ deployAddress)
                Latest
        txHash <-
          attempt 3
            $ MarketplaceSettings.markTokensAsSold
                (txOpts # _to ?~ ims)
                { _originContract, _tokenIds: filteredTokens }
        log Info $ "Batch marking tokens sold: " <> show _tokenIds
        log Info $ "Polling for markTokensAsSold transaction receipt: " <> show txHash
        eres <- try $ awaitTxSuccessWeb3 txHash
        case eres of
          Left err -> handleFailure txHash (show err) _tokenIds md
          Right _ -> handleSuccess txHash _tokenIds md
    case eres of
      Left err -> do
        log Error
          $ "Caught error batch market tokens, writing error to market details: "
          <> "\n"
          <> show err
        handleFailure nullWord (show err) _tokenIds md
      Right _ -> pure unit

batch :: forall m a b. (MonadAff m) => Int -> Array a -> (Array a -> m b) -> m (Array b)
batch batchSize xs f = for (chunk batchSize xs) f

chunk :: forall a. Int -> Array a -> Array (Array a)
chunk n = case _ of
  [] -> []
  xs -> take n xs : chunk n (drop n xs)

addressToString :: Address -> String
addressToString = toAscii <<< unAddress

tokenAndContract ::
  Address ->
  UIntN S256 ->
  { contractAddress :: Address
  , tokenId :: BigNumber
  }
tokenAndContract addr tid = { tokenId: unUIntN tid, contractAddress: addr }

-----------------------------------------------------------------------------
-- | lookUpSoldTokens
-----------------------------------------------------------------------------
type LookUpSoldTokensRes
  = { allNonFungibleTokenEvents ::
        { nodes :: Array { tokenId :: Int }
        }
    }

lookUpSoldTokens ::
  forall m.
  MonadAff m =>
  { tokenContract :: Address, url :: String, apiKey :: String } -> m (Array (UIntN S256))
lookUpSoldTokens { tokenContract, url, apiKey } = do
  res <- liftAff $ runQueryT (runQuery url (Just apiKey) gqlQuery)
  case res of
    { data: Nothing, errors: Nothing } -> liftEffect $ throw $ "No response for query \n" <> show gqlQuery
    { data: Nothing, errors: Just err } -> liftEffect $ throw $ show err
    { data: Just { allNonFungibleTokenEvents: { nodes } } } ->
      pure $ nub $ catMaybes
        $ nodes
        <#> \{ tokenId } -> uIntNFromBigNumber s256 (embed tokenId)
  where
  gqlQuery :: GraphQlQuery { contractAddress :: Address } LookUpSoldTokensRes
  gqlQuery = GraphQlQuery { query, variables: { contractAddress: tokenContract } }

  query =
    """
    query getNft($contractAddress: String!) {
      allNonFungibleTokenEvents(
        orderBy: TIMESTAMP_DESC
        condition: { tokenContractAddress: $contractAddress }
        filter: { nftEventType: { in: [SALE, ACCEPT_BID] } }
      ) {
        totalCount
        nodes {
          tokenId
        }
      }
    }
    """
