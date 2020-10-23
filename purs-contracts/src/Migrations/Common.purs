module Deploy.Contracts.Common where

import Prelude
import Chanterelle.Internal.Types (DeployConfig(..), DeployM, throwDeploy)
import Contracts.Marketplace.MarketplaceSettings as MarketplaceSettings
import Control.Monad.Reader (ask)
import Data.Either (Either(..))
import Data.Lens ((?~))
import Deploy.Utils (GasSettings, awaitTxSuccessAndLogEthStats, throwOnCallError, txOptsWithGasSettings)
import Effect.Aff.Class (liftAff)
import Effect.Exception (error)
import Network.Ethereum.Core.Signatures (Address)
import Network.Ethereum.Web3 (ChainCursor(..), HexString, _from, _to, runWeb3)

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
