module Deploy.Utils where

import Prelude
import Chanterelle.Internal.Utils (pollTransactionReceipt)
import Effect.Aff.Class (class MonadAff)
import Network.Ethereum.Core.HexString (HexString)
import Network.Ethereum.Web3 (Provider, TransactionReceipt(..), TransactionStatus(..))
import Partial.Unsafe (unsafeCrashWith)

awaitTxSuccess :: forall m. MonadAff m => HexString -> Provider -> m Unit
awaitTxSuccess txHash provider = do
  TransactionReceipt txReceipt <- pollTransactionReceipt txHash provider
  case txReceipt.status of
    Succeeded -> pure unit
    Failed -> unsafeCrashWith $ "Transaction Failed w/ hash " <> show txHash
