--------------------------------------------------------------------------------
-- | WETH9
--------------------------------------------------------------------------------

module Contracts.WETH9 where

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
import Network.Ethereum.Web3.Solidity (D2, D5, D6, D8, DOne, Tuple0(..), Tuple1(..), Tuple2(..), Tuple3(..), UIntN, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Network.Ethereum.Web3.Types.TokenUnit (MinorUnit)
import Partial.Unsafe (unsafePartial)


--------------------------------------------------------------------------------
-- | Approval
--------------------------------------------------------------------------------


newtype Approval = Approval {_owner :: Address,_spender :: Address,_value :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeApproval :: Newtype Approval _

instance eventFilterApproval :: EventFilter Approval where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "8c5be1e5ebec7d5bd14f71427d1e84f3dd0314c0f7b2291e5b200ac8c7c3b925"),Nothing,Nothing]

instance indexedEventApproval :: IndexedEvent (Tuple2 (Tagged (SProxy "_owner") Address) (Tagged (SProxy "_spender") Address)) (Tuple1 (Tagged (SProxy "_value") (UIntN (D2 :& D5 :& DOne D6)))) Approval where
  isAnonymous _ = false

derive instance genericApproval :: Generic Approval _

instance eventGenericApprovalShow :: Show Approval where
  show = genericShow

instance eventGenericApprovaleq :: Eq Approval where
  eq = genericEq

--------------------------------------------------------------------------------
-- | Transfer
--------------------------------------------------------------------------------


newtype Transfer = Transfer {_from :: Address,_to :: Address,_value :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeTransfer :: Newtype Transfer _

instance eventFilterTransfer :: EventFilter Transfer where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "ddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef"),Nothing,Nothing]

instance indexedEventTransfer :: IndexedEvent (Tuple2 (Tagged (SProxy "_from") Address) (Tagged (SProxy "_to") Address)) (Tuple1 (Tagged (SProxy "_value") (UIntN (D2 :& D5 :& DOne D6)))) Transfer where
  isAnonymous _ = false

derive instance genericTransfer :: Generic Transfer _

instance eventGenericTransferShow :: Show Transfer where
  show = genericShow

instance eventGenericTransfereq :: Eq Transfer where
  eq = genericEq

--------------------------------------------------------------------------------
-- | Deposit
--------------------------------------------------------------------------------


newtype Deposit = Deposit {_owner :: Address,_value :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeDeposit :: Newtype Deposit _

instance eventFilterDeposit :: EventFilter Deposit where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "e1fffcc4923d04b559f4d29a8bfc6cda04eb5b0d3c460751c2402c5c5cc9109c"),Nothing]

instance indexedEventDeposit :: IndexedEvent (Tuple1 (Tagged (SProxy "_owner") Address)) (Tuple1 (Tagged (SProxy "_value") (UIntN (D2 :& D5 :& DOne D6)))) Deposit where
  isAnonymous _ = false

derive instance genericDeposit :: Generic Deposit _

instance eventGenericDepositShow :: Show Deposit where
  show = genericShow

instance eventGenericDepositeq :: Eq Deposit where
  eq = genericEq

--------------------------------------------------------------------------------
-- | Withdrawal
--------------------------------------------------------------------------------


newtype Withdrawal = Withdrawal {_owner :: Address,_value :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeWithdrawal :: Newtype Withdrawal _

instance eventFilterWithdrawal :: EventFilter Withdrawal where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "7fcf532c15f0a6db0bd6d0e038bea71d30d808c7d98cb3bf7268a95bf5081b65"),Nothing]

instance indexedEventWithdrawal :: IndexedEvent (Tuple1 (Tagged (SProxy "_owner") Address)) (Tuple1 (Tagged (SProxy "_value") (UIntN (D2 :& D5 :& DOne D6)))) Withdrawal where
  isAnonymous _ = false

derive instance genericWithdrawal :: Generic Withdrawal _

instance eventGenericWithdrawalShow :: Show Withdrawal where
  show = genericShow

instance eventGenericWithdrawaleq :: Eq Withdrawal where
  eq = genericEq

--------------------------------------------------------------------------------
-- | AllowanceFn
--------------------------------------------------------------------------------


type AllowanceFn = Tagged (SProxy "allowance(address,address)") (Tuple2 Address Address)

allowance :: TransactionOptions NoPay -> ChainCursor -> Address -> Address -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
allowance x0 cm x2 x3 = map unTuple1 <$> call x0 cm ((tagged $ Tuple2 x2 x3) :: AllowanceFn)

--------------------------------------------------------------------------------
-- | ApproveFn
--------------------------------------------------------------------------------


type ApproveFn = Tagged (SProxy "approve(address,uint256)") (Tuple2 (Tagged (SProxy "guy") Address) (Tagged (SProxy "wad") (UIntN (D2 :& D5 :& DOne D6))))

approve :: TransactionOptions NoPay -> { guy :: Address, wad :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
approve x0 r = uncurryFields  r $ approve' x0
   where
    approve' :: TransactionOptions NoPay -> (Tagged (SProxy "guy") Address) -> (Tagged (SProxy "wad") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    approve' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 y1 y2) :: ApproveFn)

--------------------------------------------------------------------------------
-- | BalanceOfFn
--------------------------------------------------------------------------------


type BalanceOfFn = Tagged (SProxy "balanceOf(address)") (Tuple1 Address)

balanceOf :: TransactionOptions NoPay -> ChainCursor -> Address -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
balanceOf x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: BalanceOfFn)

--------------------------------------------------------------------------------
-- | DecimalsFn
--------------------------------------------------------------------------------


type DecimalsFn = Tagged (SProxy "decimals()") (Tuple0 )

decimals :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN (DOne D8)))
decimals x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: DecimalsFn)

--------------------------------------------------------------------------------
-- | DepositFn
--------------------------------------------------------------------------------


type DepositFn = Tagged (SProxy "deposit()") (Tuple0 )

deposit :: TransactionOptions MinorUnit -> Web3 HexString
deposit x0 = sendTx x0 ((tagged $ Tuple0 ) :: DepositFn)

--------------------------------------------------------------------------------
-- | NameFn
--------------------------------------------------------------------------------


type NameFn = Tagged (SProxy "name()") (Tuple0 )

name :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError String)
name x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: NameFn)

--------------------------------------------------------------------------------
-- | SymbolFn
--------------------------------------------------------------------------------


type SymbolFn = Tagged (SProxy "symbol()") (Tuple0 )

symbol :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError String)
symbol x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: SymbolFn)

--------------------------------------------------------------------------------
-- | TotalSupplyFn
--------------------------------------------------------------------------------


type TotalSupplyFn = Tagged (SProxy "totalSupply()") (Tuple0 )

totalSupply :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
totalSupply x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: TotalSupplyFn)

--------------------------------------------------------------------------------
-- | TransferFn
--------------------------------------------------------------------------------


type TransferFn = Tagged (SProxy "transfer(address,uint256)") (Tuple2 (Tagged (SProxy "dst") Address) (Tagged (SProxy "wad") (UIntN (D2 :& D5 :& DOne D6))))

transfer :: TransactionOptions NoPay -> { dst :: Address, wad :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
transfer x0 r = uncurryFields  r $ transfer' x0
   where
    transfer' :: TransactionOptions NoPay -> (Tagged (SProxy "dst") Address) -> (Tagged (SProxy "wad") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    transfer' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 y1 y2) :: TransferFn)

--------------------------------------------------------------------------------
-- | TransferFromFn
--------------------------------------------------------------------------------


type TransferFromFn = Tagged (SProxy "transferFrom(address,address,uint256)") (Tuple3 (Tagged (SProxy "src") Address) (Tagged (SProxy "dst") Address) (Tagged (SProxy "wad") (UIntN (D2 :& D5 :& DOne D6))))

transferFrom :: TransactionOptions NoPay -> { src :: Address, dst :: Address, wad :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
transferFrom x0 r = uncurryFields  r $ transferFrom' x0
   where
    transferFrom' :: TransactionOptions NoPay -> (Tagged (SProxy "src") Address) -> (Tagged (SProxy "dst") Address) -> (Tagged (SProxy "wad") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    transferFrom' y0 y1 y2 y3 = sendTx y0 ((tagged $ Tuple3 y1 y2 y3) :: TransferFromFn)

--------------------------------------------------------------------------------
-- | WithdrawFn
--------------------------------------------------------------------------------


type WithdrawFn = Tagged (SProxy "withdraw(uint256)") (Tuple1 (Tagged (SProxy "wad") (UIntN (D2 :& D5 :& DOne D6))))

withdraw :: TransactionOptions NoPay -> { wad :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
withdraw x0 r = uncurryFields  r $ withdraw' x0
   where
    withdraw' :: TransactionOptions NoPay -> (Tagged (SProxy "wad") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    withdraw' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: WithdrawFn)