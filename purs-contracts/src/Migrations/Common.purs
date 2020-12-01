module Deploy.Contracts.Common where

import Prelude
import Chanterelle.Internal.Types (DeployConfig(..), DeployM, throwDeploy)
import Contracts.Marketplace.MarketplaceSettings as MarketplaceSettings
import Control.Monad.Reader (ask)
import Data.Array (catMaybes, drop, nub, take, (:))
import Data.Either (Either(..))
import Data.Lens ((?~))
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Deploy.Utils (GasSettings, awaitTxSuccessAndLogEthStats, defaultTxOptions, throwOnCallError, txOptsWithGasSettings)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (error, throw)
import Network.Ethereum.Core.HexString (toAscii)
import Network.Ethereum.Core.Signatures (Address, unAddress)
import Network.Ethereum.Web3 (ChainCursor(..), HexString, UIntN, _from, _to, embed, runWeb3, uIntNFromBigNumber)
import Network.Ethereum.Web3.Solidity.Sizes (S256, s256)
import Simple.Graphql.Query (runQuery)
import Simple.Graphql.Types (GraphQlQuery(..), runQueryT)

-----------------------------------------------------------------------------
--- | setMarketplaceWithTokenMarkRole
-----------------------------------------------------------------------------
setMarketplaceWithTokenMarkRole :: GasSettings -> Address -> Address -> DeployM HexString
setMarketplaceWithTokenMarkRole gs settingsAddress granteeAddress = do
  (DeployConfig { provider, networkID, primaryAccount, writeArtifacts }) <- ask
  etxHash <-
    liftAff
      $ runWeb3 provider do
          tokenMarkRole <- throwOnCallError $ MarketplaceSettings.tOKEN_MARK_ROLE (txOptsWithGasSettings gs # _to ?~ settingsAddress) Latest
          txHash <-
            MarketplaceSettings.grantRole
              ( txOptsWithGasSettings gs
                  # _from
                  ?~ primaryAccount
                  # _to
                  ?~ settingsAddress
              )
              { account: granteeAddress, role: tokenMarkRole }
          awaitTxSuccessAndLogEthStats txHash
          pure txHash
  case etxHash of
    Left err ->
      throwDeploy $ error
        $ "Failed setting "
        <> show granteeAddress
        <> " for token mark role with error: "
        <> show err
    Right txHash -> pure txHash

-----------------------------------------------------------------------------
--- | lookUpSoldTokens
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

-----------------------------------------------------------------------------
--- | filterAlreadyMarkedSoldTokens
-----------------------------------------------------------------------------
filterAlreadyMarkedSoldTokens ::
  Address -> Address -> Array (UIntN S256) -> DeployM (Array (UIntN S256))
filterAlreadyMarkedSoldTokens settingsAddress _contractAddress tids = do
  DeployConfig { provider } <- ask
  catMaybes
    <$> for tids \_tokenId -> do
        ehasSold <-
          liftAff
            $ runWeb3 provider
            $ throwOnCallError
            $ MarketplaceSettings.hasERC721TokenSold
                (defaultTxOptions # _to ?~ settingsAddress)
                Latest
                { _contractAddress, _tokenId }
        case ehasSold of
          Left err ->
            throwDeploy $ error
              $ "Failed filtering already sold tokens"
              <> show err
          Right true -> pure Nothing
          Right false -> pure (Just _tokenId)

-----------------------------------------------------------------------------
--- | filterAlreadyMarkedSoldTokens
-----------------------------------------------------------------------------
batchMarkSold ::
  Address -> Address -> Array (UIntN S256) -> DeployM (Array (UIntN S256))
batchMarkSold settingsAddress _contractAddress tids = do
  DeployConfig { provider } <- ask
  catMaybes
    <$> for tids \_tokenId -> do
        ehasSold <-
          liftAff
            $ runWeb3 provider
            $ throwOnCallError
            $ MarketplaceSettings.hasERC721TokenSold
                (defaultTxOptions # _to ?~ settingsAddress)
                Latest
                { _contractAddress, _tokenId }
        case ehasSold of
          Left err ->
            throwDeploy $ error
              $ "Failed filtering already sold tokens"
              <> show err
          Right true -> pure Nothing
          Right false -> pure (Just _tokenId)
  where
  batch :: forall m a b. (MonadAff m) => Int -> Array a -> (Array a -> m b) -> m (Array b)
  batch batchSize xs f = for (chunk batchSize xs) f

  chunk :: forall a. Int -> Array a -> Array (Array a)
  chunk n = case _ of
    [] -> []
    xs -> take n xs : chunk n (drop n xs)

  addressToString :: Address -> String
  addressToString = toAscii <<< unAddress
