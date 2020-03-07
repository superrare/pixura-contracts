--------------------------------------------------------------------------------
-- | Operated
--------------------------------------------------------------------------------

module Contracts.Operated where

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
import Network.Ethereum.Web3 (_address, _topics, call, class EventFilter, deployContract, sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (D2, D5, D6, DOne, Tuple0(..), Tuple1(..), Tuple2(..), UIntN, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
--------------------------------------------------------------------------------
-- | ConstructorFn
--------------------------------------------------------------------------------


type ConstructorFn = Tagged (SProxy "constructor(address,uint256)") (Tuple2 (Tagged (SProxy "_operator") Address) (Tagged (SProxy "_operationCost") (UIntN (D2 :& D5 :& DOne D6))))

constructor :: TransactionOptions NoPay -> HexString -> { _operator :: Address, _operationCost :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
constructor x0 bc r = uncurryFields  r $ constructor' x0 bc
   where
    constructor' :: TransactionOptions NoPay -> HexString -> (Tagged (SProxy "_operator") Address) -> (Tagged (SProxy "_operationCost") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    constructor' y0 bc' y2 y3 = deployContract y0 bc' ((tagged $ Tuple2 y2 y3) :: ConstructorFn)

--------------------------------------------------------------------------------
-- | OperatorSet
--------------------------------------------------------------------------------


newtype OperatorSet = OperatorSet {_operator :: Address}

derive instance newtypeOperatorSet :: Newtype OperatorSet _

instance eventFilterOperatorSet :: EventFilter OperatorSet where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "99d737e0adf2c449d71890b86772885ec7959b152ddb265f76325b6e68e105d3"),Nothing]

instance indexedEventOperatorSet :: IndexedEvent (Tuple1 (Tagged (SProxy "_operator") Address)) (Tuple0 ) OperatorSet where
  isAnonymous _ = false

derive instance genericOperatorSet :: Generic OperatorSet _

instance eventGenericOperatorSetShow :: Show OperatorSet where
  show = genericShow

instance eventGenericOperatorSeteq :: Eq OperatorSet where
  eq = genericEq

--------------------------------------------------------------------------------
-- | OperationCostSet
--------------------------------------------------------------------------------


newtype OperationCostSet = OperationCostSet {_operationCost :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeOperationCostSet :: Newtype OperationCostSet _

instance eventFilterOperationCostSet :: EventFilter OperationCostSet where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "70524409f1852b9903371526eb3765eb5462f72a3df57469113284ab8a76ec95"),Nothing]

instance indexedEventOperationCostSet :: IndexedEvent (Tuple1 (Tagged (SProxy "_operationCost") (UIntN (D2 :& D5 :& DOne D6)))) (Tuple0 ) OperationCostSet where
  isAnonymous _ = false

derive instance genericOperationCostSet :: Generic OperationCostSet _

instance eventGenericOperationCostSetShow :: Show OperationCostSet where
  show = genericShow

instance eventGenericOperationCostSeteq :: Eq OperationCostSet where
  eq = genericEq

--------------------------------------------------------------------------------
-- | OperationCostFn
--------------------------------------------------------------------------------


type OperationCostFn = Tagged (SProxy "operationCost()") (Tuple0 )

operationCost :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
operationCost x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: OperationCostFn)

--------------------------------------------------------------------------------
-- | OperatorFn
--------------------------------------------------------------------------------


type OperatorFn = Tagged (SProxy "operator()") (Tuple0 )

operator :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Address)
operator x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: OperatorFn)

--------------------------------------------------------------------------------
-- | RemoveOperatorFn
--------------------------------------------------------------------------------


type RemoveOperatorFn = Tagged (SProxy "removeOperator()") (Tuple0 )

removeOperator :: TransactionOptions NoPay -> Web3 HexString
removeOperator x0 = sendTx x0 ((tagged $ Tuple0 ) :: RemoveOperatorFn)

--------------------------------------------------------------------------------
-- | SetOperationCostFn
--------------------------------------------------------------------------------


type SetOperationCostFn = Tagged (SProxy "setOperationCost(uint256)") (Tuple1 (Tagged (SProxy "_operationCost") (UIntN (D2 :& D5 :& DOne D6))))

setOperationCost :: TransactionOptions NoPay -> { _operationCost :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
setOperationCost x0 r = uncurryFields  r $ setOperationCost' x0
   where
    setOperationCost' :: TransactionOptions NoPay -> (Tagged (SProxy "_operationCost") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    setOperationCost' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: SetOperationCostFn)

--------------------------------------------------------------------------------
-- | SetOperatorFn
--------------------------------------------------------------------------------


type SetOperatorFn = Tagged (SProxy "setOperator(address)") (Tuple1 (Tagged (SProxy "_operator") Address))

setOperator :: TransactionOptions NoPay -> { _operator :: Address } -> Web3 HexString
setOperator x0 r = uncurryFields  r $ setOperator' x0
   where
    setOperator' :: TransactionOptions NoPay -> (Tagged (SProxy "_operator") Address) -> Web3 HexString
    setOperator' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: SetOperatorFn)