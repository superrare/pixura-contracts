module Migrations.SuperRareAuctionHouse where

import Prelude
import Chanterelle.Internal.Types (DeployConfig(..), DeployM, throwDeploy)
import Contracts.Marketplace.MarketplaceSettings as MarketplaceSettings
import Control.Monad.Reader (ask)
import Data.Either (Either(..))
import Data.Lens ((?~))
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Deploy.Contracts.Common (setMarketplaceWithTokenMarkRole)
import Deploy.Contracts.SuperRareAuctionHouse (deployScriptWithGasSettings)
import Deploy.Utils (awaitTxSuccessAndLogEthStats, txOptsWithGasSettings)
import Effect (Effect)
import Effect.Aff (error)
import Effect.Aff.Class (liftAff)
import Migrations.Utils (MigrationSettings, emptyGasSettings, runMigration)
import Network.Ethereum.Core.HexString (HexString)
import Network.Ethereum.Web3 (Address, _from, _to, runWeb3)
import Network.Ethereum.Web3.Solidity.Sizes (s8)
import Partial.Unsafe (unsafePartial)
import Test.Spec.Contracts.Utils (intToUIntN)

type MigrationArgs
  = { iMarketSettings :: Address
    , iERC721RoyaltySettings :: Address
    }

type MigrationProgress
  = { superRareAuctionHouse :: Maybe Address
    , setAuctionHouseRoleForMarkingSoldTx :: Maybe HexString
    }

emptyMigrationProgress :: MigrationProgress
emptyMigrationProgress =
  { superRareAuctionHouse: Nothing
  , setAuctionHouseRoleForMarkingSoldTx: Nothing
  }

main :: Effect Unit
main = runMigration emptyMigrationProgress migration

migration :: MigrationSettings MigrationArgs MigrationProgress -> DeployM Unit
migration { migrationArgs, getProgress, gasSettings: mgs, updateProgress } = do
  DeployConfig { provider, primaryAccount } <- ask
  let
    gasSettings = fromMaybe emptyGasSettings mgs
  deployContract gasSettings
  where
  -- Get the Auction House address
  getSuperRareAuctionHouse = getProgress >>= \{ superRareAuctionHouse } -> pure $ unsafePartial fromJust superRareAuctionHouse

  -- Deploy the contract
  deployContract gasSettings =
    getProgress
      >>= case _ of
          { superRareAuctionHouse: Just _ } -> pure unit
          { superRareAuctionHouse: Nothing } -> do
            let
              { iERC721RoyaltySettings, iMarketSettings } = migrationArgs
            { superRareAuctionHouse: { deployAddress } } <-
              deployScriptWithGasSettings gasSettings
                { _iERC721CreatorRoyalty: iERC721RoyaltySettings, _iMarketSettings: iMarketSettings }
            updateProgress \prog -> prog { superRareAuctionHouse = Just deployAddress }
            pure unit

  -- Set AuctionHouse with permisssion to set tokens for sale
  setAuctionHouseRoleForMarkingSold gasSettings =
    getProgress
      >>= case _ of
          { setAuctionHouseRoleForMarkingSoldTx: Just _ } -> pure unit
          { setAuctionHouseRoleForMarkingSoldTx: Nothing } -> do
            superRareAuctionHouse <- getSuperRareAuctionHouse
            DeployConfig { provider, primaryAccount } <- ask
            let
              { iMarketSettings } = migrationArgs
            txHash <- setMarketplaceWithTokenMarkRole gasSettings iMarketSettings superRareAuctionHouse
            updateProgress \prog -> prog { setAuctionHouseRoleForMarkingSoldTx = Just txHash }
