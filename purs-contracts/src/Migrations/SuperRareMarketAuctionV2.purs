module Migrations.SuperRareMarketAuctionV2 where

import Prelude
import Chanterelle.Internal.Logging (LogLevel(..), log)
import Chanterelle.Internal.Types (DeployConfig(..))
import Contracts.V5.SuperRareMarketAuctionV2 (markTokensAsSold)
import Control.Monad.Reader (ask)
import Data.Array (catMaybes, drop, nub, take, (:))
import Data.Either (Either(..), either)
import Data.Lens ((?~))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for)
import Deploy.Contracts.SuperRareMarketAuctionV2 (deployScriptWithGasSettings)
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
  = { superRareV2ContractAddress :: Address
    , pixuraApi :: { url :: String, apiKey :: String }
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

          { superRareV2ContractAddress: _originContract
          , pixuraApi: { url, apiKey }
          } = migrationArgs

          gasSettings = fromMaybe emptyGasSettings mgs

          txOpts = txOptsWithGasSettings gasSettings # _from ?~ primaryAccount
        fibTokens <- liftEffect $ launchAff (lookUpSoldTokens { tokenContract: _originContract, url, apiKey })
        { superRareMarketAuctionV2: { deployAddress } } <- deployScriptWithGasSettings gasSettings
        res <-
          liftAff
            $ runWeb3 provider do
                soldTokens <- liftAff $ joinFiber fibTokens
                for (chunk 100 soldTokens) \_tokenIds -> do
                  txHash <-
                    markTokensAsSold
                      (txOpts # _to ?~ deployAddress)
                      { _originContract, _tokenIds }
                  log Info $ "Batch marking tokens sold: " <> show _tokenIds
                  log Info $ "Polling for markTokensAsSold transaction receipt: " <> show txHash
                  awaitTxSuccessWeb3 txHash
        case res of
          Left err -> liftEffect $ throw $ show err
          _ -> pure unit
  where
  chunk n [] = []

  chunk n xs = take n xs : chunk n (drop n xs)

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
