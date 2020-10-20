--------------------------------------------------------------------------------
-- | SendValueOrEscrow
--------------------------------------------------------------------------------

module Contracts.SendValueOrEscrow where

import Prelude 

import Data.Either (Either)
import Data.Functor.Tagged (Tagged, tagged)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens ((.~))
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (_address, _topics, call, class EventFilter, sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (D2, D5, D6, DOne, Tuple1(..), UIntN, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
--------------------------------------------------------------------------------
-- | SendValue
--------------------------------------------------------------------------------


newtype SendValue = SendValue {_payee :: Address,amount :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeSendValue :: Newtype SendValue _

instance eventFilterSendValue :: EventFilter SendValue where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "d88d14434dd4c47d8cd227a931f475ddfaa46a219994f6c5094bd04d940c3c1b"),Nothing]

instance indexedEventSendValue :: IndexedEvent (Tuple1 (Tagged (SProxy "_payee") Address)) (Tuple1 (Tagged (SProxy "amount") (UIntN (D2 :& D5 :& DOne D6)))) SendValue where
  isAnonymous _ = false

derive instance genericSendValue :: Generic SendValue _

instance eventGenericSendValueShow :: Show SendValue where
  show = genericShow

instance eventGenericSendValueeq :: Eq SendValue where
  eq = genericEq

--------------------------------------------------------------------------------
-- | PaymentsFn
--------------------------------------------------------------------------------


type PaymentsFn = Tagged (SProxy "payments(address)") (Tuple1 (Tagged (SProxy "dest") Address))

payments :: TransactionOptions NoPay -> ChainCursor -> { dest :: Address } -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
payments x0 cm r = uncurryFields  r $ payments' x0 cm
   where
    payments' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "dest") Address) -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
    payments' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 y2) :: PaymentsFn)

--------------------------------------------------------------------------------
-- | WithdrawPaymentsFn
--------------------------------------------------------------------------------


type WithdrawPaymentsFn = Tagged (SProxy "withdrawPayments(address)") (Tuple1 (Tagged (SProxy "payee") Address))

withdrawPayments :: TransactionOptions NoPay -> { payee :: Address } -> Web3 HexString
withdrawPayments x0 r = uncurryFields  r $ withdrawPayments' x0
   where
    withdrawPayments' :: TransactionOptions NoPay -> (Tagged (SProxy "payee") Address) -> Web3 HexString
    withdrawPayments' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: WithdrawPaymentsFn)