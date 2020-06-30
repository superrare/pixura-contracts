module Migrations.SuperRareLegacy where

import Prelude
import Chanterelle.Internal.Logging (LogLevel(..), log)
import Chanterelle.Internal.Types (DeployConfig(..), throwDeploy)
import Control.Monad.Reader (ask)
import Data.Array (catMaybes, concat, drop, elem, filter, nub, take, (:))
import Data.Either (Either(..), either)
import Data.Lens ((?~))
import Data.Maybe (Maybe(..), fromMaybe)
import Deploy.Contracts.SuperRareLegacy (deployScriptWithGasSettings, emptyMintingDetails, mintLegacyTokens)
import Deploy.Utils (GasSettings, txOptsWithGasSettings)
import Effect (Effect)
import Effect.Aff (error, runAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (throw, throwException)
import Migrations.Utils (emptyGasSettings, runMigration)
import Network.Ethereum.Web3 (Address, BigNumber, _from, embed, uIntNFromBigNumber)
import Network.Ethereum.Web3.Solidity.Sizes (s256)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Process (cwd)
import Simple.Graphql.Query (runQuery)
import Simple.Graphql.Types (GraphQlQuery(..), runQueryT)
import Simple.JSON (readJSON, writeJSON)

type MigrationArgs
  = { oldSuperRare :: Address
    , pixuraApi :: { url :: String, apiKey :: String }
    , mintingDetailsFile :: Maybe String
    }

main :: Effect Unit
main =
  runAff_ (either throwException (const $ log Info "Completed Migration"))
    $ runMigration \(args :: { gasSettings :: Maybe GasSettings, migrationArgs :: MigrationArgs }) -> do
        DeployConfig { provider, primaryAccount } <- ask
        cwd' <- liftEffect cwd
        let
          { migrationArgs
          , gasSettings: mgs
          } = args

          { oldSuperRare
          , pixuraApi: { url, apiKey }
          , mintingDetailsFile
          } = migrationArgs

          gasSettings = fromMaybe emptyGasSettings mgs

          txOpts = txOptsWithGasSettings gasSettings # _from ?~ primaryAccount

          writeFilename = fromMaybe (cwd' <> "/minting-details.json") migrationArgs.mintingDetailsFile

          readJSONFile n = do
            t <- liftAff $ FS.readTextFile UTF8 n
            case readJSON t of
              Left err -> throwDeploy (error $ show err)
              Right v -> pure v
        { legacyTokenAddress, mintingDetails: md } <- case migrationArgs.mintingDetailsFile of
          Nothing -> do
            { superRareLegacy: { deployAddress } } <-
              deployScriptWithGasSettings gasSettings
                { _name: "SuperRareLegacy"
                , _symbol: "SUPR"
                , _oldSuperRare: oldSuperRare
                }
            pure
              { legacyTokenAddress: deployAddress
              , mintingDetails: emptyMintingDetails deployAddress
              }
          Just n -> do
            mintingDetails <- readJSONFile n
            pure { legacyTokenAddress: mintingDetails.contractAddress, mintingDetails }
        tokenIds <- do
          tids <- lookUpTokenIds { tokenContract: oldSuperRare, url, apiKey }
          let
            completed = concat $ md.successfulTransactions <#> \{ tokenIds } -> tokenIds

            filteredIds = filter (\tid -> not $ elem tid completed) tids
          pure $ nub $ catMaybes $ filteredIds <#> \tid -> uIntNFromBigNumber s256 tid
        md' <- mintLegacyTokens { primaryAccount, provider, mintingDetails: Just md } gasSettings tokenIds md.contractAddress
        log Info $ "Writing results to: " <> writeFilename
        liftAff $ FS.writeTextFile UTF8 writeFilename (writeJSON md')
        log Info $ "Minting Results:\n" <> (writeJSON md')
  where
  chunk n [] = []

  chunk n xs = take n xs : chunk n (drop n xs)

type LookUpTokenIdsRes
  = { allNonFungibleTokens ::
        { nodes :: Array { tokenId :: Int }
        }
    }

lookUpTokenIds ::
  forall m.
  MonadAff m =>
  { tokenContract :: Address, url :: String, apiKey :: String } -> m (Array BigNumber)
lookUpTokenIds { tokenContract, url, apiKey } = do
  res <- liftAff $ runQueryT (runQuery url (Just apiKey) gqlQuery)
  case res of
    { data: Nothing, errors: Nothing } ->
      liftEffect
        $ throw
        $ "No response for query \n"
        <> show gqlQuery
    { data: Nothing, errors: Just err } -> liftEffect $ throw $ show err
    { data: Just { allNonFungibleTokens: { nodes } } } ->
      pure
        $ nub
        $ nodes
        <#> \{ tokenId } -> embed tokenId
  where
  gqlQuery :: GraphQlQuery { contractAddress :: Address } LookUpTokenIdsRes
  gqlQuery = GraphQlQuery { query, variables: { contractAddress: tokenContract } }

  query =
    """
    query getNft($contractAddress: String!) {
      allNonFungibleTokens(
        orderBy: TOKEN_ID_ASC
        condition: { contractAddress: $contractAddress, burned:false }
      ) {
        totalCount
        nodes {
          tokenId
        }
      }
    }
    """
