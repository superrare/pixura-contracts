module Migrations.SuperRareMarketAuctionV2 where

import Prelude
import Chanterelle.Internal.Logging (LogLevel(..), log)
import Chanterelle.Internal.Types (DeployConfig(..))
import Control.Monad.Reader (ask)
import Data.Array (catMaybes, drop, nub, take, (:))
import Data.Either (Either(..), either)
import Data.Lens ((?~))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Traversable (for)
import Deploy.Contracts.SuperRareLegacy (deployScriptWithGasSettings)
import Deploy.Utils (GasSettings, awaitTxSuccessWeb3, txOptsWithGasSettings)
import Effect (Effect)
import Effect.Aff (joinFiber, launchAff, runAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (throw, throwException)
import Migrations.Utils (emptyGasSettings, runMigration)
import Network.Ethereum.Web3 (Address, UIntN, _from, _to, embed, runWeb3, uIntNFromBigNumber)
import Network.Ethereum.Web3.Solidity.Sizes (S256, s256)
import Simple.Graphql.Query (runQuery)
import Simple.Graphql.Types (GraphQlQuery(..), runQueryT)

type MigrationArgs
  = { oldSuperRare :: Address
    , pixuraApi :: { url :: String, apiKey :: String }
    , tokenIds :: Maybe (Array Int)
    }

main :: Effect Unit
main =
  runAff_ (either throwException (const $ log Info "Completed Migration"))
    $ runMigration \(args :: { gasSettings :: Maybe GasSettings, migrationArgs :: MigrationArgs }) -> do
        DeployConfig { provider, primaryAccount } <- ask
        let
          { migrationArgs
          , gasSettings: mgs
          } = args

          { oldSuperRare
          , pixuraApi: { url, apiKey }
          , tokenIds: mTokenIds
          } = migrationArgs

          gasSettings = fromMaybe emptyGasSettings mgs

          txOpts = txOptsWithGasSettings gasSettings # _from ?~ primaryAccount
        tokenIds <- case mTokenIds of
          Nothing -> lookUpTokenIds { tokenContract: oldSuperRare, url, apiKey }
          Just tids -> pure $ nub $ catMaybes $ tids <#> \tid -> uIntNFromBigNumber s256 (embed tid)
        void
          $ deployScriptWithGasSettings gasSettings tokenIds
              { _name: "SuperRareLegacy"
              , _symbol: "SUPR"
              , _oldSuperRare: oldSuperRare
              }
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
  { tokenContract :: Address, url :: String, apiKey :: String } -> m (Array (UIntN S256))
lookUpTokenIds { tokenContract, url, apiKey } = do
  res <- liftAff $ runQueryT (runQuery url (Just apiKey) gqlQuery)
  case res of
    { data: Nothing, errors: Nothing } -> liftEffect $ throw $ "No response for query \n" <> show gqlQuery
    { data: Nothing, errors: Just err } -> liftEffect $ throw $ show err
    { data: Just { allNonFungibleTokens: { nodes } } } ->
      pure $ nub $ catMaybes
        $ nodes
        <#> \{ tokenId } -> uIntNFromBigNumber s256 (embed tokenId)
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
