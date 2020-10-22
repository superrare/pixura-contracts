module Deploy.Contracts.Common where

import Prelude
import Chanterelle.Internal.Types (DeployConfig(..), DeployError(..), DeployM(..))
import Contracts.Marketplace.MarketplaceSettings as MarketplaceSettings
import Control.Comonad.Env (ask)
import Data.Lens ((.~), (?~))
import Deploy.Utils (GasSettings(..), awaitTxSuccessWeb3, throwOnCallError, txOptsWithGasSettings)
import Network.Ethereum.Core.Signatures (Address)
import Network.Ethereum.Web3 (ChainCursor(..), _to, runWeb3)

setMarketplaceWithTokenMarkRole :: GasSettings -> Address -> Address -> DeployM Unit
setMarketplaceWithTokenMarkRole gs settingsAddress granteeAddress = do
  (DeployConfig { provider, networkID, primaryAccount, writeArtifacts }) <- ask
  withExceptM' onDeploymentError <<< liftAff
    $ runWeb3 provider do
        tokenMarkRole <- throwOnCallError $ MarketplaceSettings.tOKEN_MARK_ROLE (txOptsWithGasSettings gs # _to ?~ settingsAddress) Latest
        txHash <-
          MarketplaceSettings.grantRole (txOptsWithGasSettings gs # _to ?~ settingsAddress)
            { account: granteeAddress, role: tokenMarkRole }
        awaitTxSuccessWeb3 txHash
