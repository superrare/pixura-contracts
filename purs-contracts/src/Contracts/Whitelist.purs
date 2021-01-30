--------------------------------------------------------------------------------
-- | Whitelist
--------------------------------------------------------------------------------

module Contracts.Whitelist where

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
import Network.Ethereum.Web3.Solidity (Tuple0(..), Tuple1(..), Tuple2, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
--------------------------------------------------------------------------------
-- | AddToWhitelist
--------------------------------------------------------------------------------


newtype AddToWhitelist = AddToWhitelist {_newAddress :: Address}

derive instance newtypeAddToWhitelist :: Newtype AddToWhitelist _

instance eventFilterAddToWhitelist :: EventFilter AddToWhitelist where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "75b2135d1c8c3519f3c09c43fe6527089ef09f40c7981ebf0ed46e79e79032c7"),Nothing]

instance indexedEventAddToWhitelist :: IndexedEvent (Tuple1 (Tagged (SProxy "_newAddress") Address)) (Tuple0 ) AddToWhitelist where
  isAnonymous _ = false

derive instance genericAddToWhitelist :: Generic AddToWhitelist _

instance eventGenericAddToWhitelistShow :: Show AddToWhitelist where
  show = genericShow

instance eventGenericAddToWhitelisteq :: Eq AddToWhitelist where
  eq = genericEq

--------------------------------------------------------------------------------
-- | RemoveFromWhitelist
--------------------------------------------------------------------------------


newtype RemoveFromWhitelist = RemoveFromWhitelist {_removedAddress :: Address}

derive instance newtypeRemoveFromWhitelist :: Newtype RemoveFromWhitelist _

instance eventFilterRemoveFromWhitelist :: EventFilter RemoveFromWhitelist where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "1f756c8b089af6b33ee121fee8badac2553a2fa89c0575ea91ff8792617746c2"),Nothing]

instance indexedEventRemoveFromWhitelist :: IndexedEvent (Tuple1 (Tagged (SProxy "_removedAddress") Address)) (Tuple0 ) RemoveFromWhitelist where
  isAnonymous _ = false

derive instance genericRemoveFromWhitelist :: Generic RemoveFromWhitelist _

instance eventGenericRemoveFromWhitelistShow :: Show RemoveFromWhitelist where
  show = genericShow

instance eventGenericRemoveFromWhitelisteq :: Eq RemoveFromWhitelist where
  eq = genericEq

--------------------------------------------------------------------------------
-- | OwnershipTransferred
--------------------------------------------------------------------------------


newtype OwnershipTransferred = OwnershipTransferred {previousOwner :: Address,newOwner :: Address}

derive instance newtypeOwnershipTransferred :: Newtype OwnershipTransferred _

instance eventFilterOwnershipTransferred :: EventFilter OwnershipTransferred where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "8be0079c531659141344cd1fd0a4f28419497f9722a3daafe3b4186f6b6457e0"),Nothing,Nothing]

instance indexedEventOwnershipTransferred :: IndexedEvent (Tuple2 (Tagged (SProxy "previousOwner") Address) (Tagged (SProxy "newOwner") Address)) (Tuple0 ) OwnershipTransferred where
  isAnonymous _ = false

derive instance genericOwnershipTransferred :: Generic OwnershipTransferred _

instance eventGenericOwnershipTransferredShow :: Show OwnershipTransferred where
  show = genericShow

instance eventGenericOwnershipTransferredeq :: Eq OwnershipTransferred where
  eq = genericEq

--------------------------------------------------------------------------------
-- | AddToWhitelistFn
--------------------------------------------------------------------------------


type AddToWhitelistFn = Tagged (SProxy "addToWhitelist(address)") (Tuple1 (Tagged (SProxy "_newAddress") Address))

addToWhitelist :: TransactionOptions NoPay -> { _newAddress :: Address } -> Web3 HexString
addToWhitelist x0 r = uncurryFields  r $ addToWhitelist' x0
   where
    addToWhitelist' :: TransactionOptions NoPay -> (Tagged (SProxy "_newAddress") Address) -> Web3 HexString
    addToWhitelist' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: AddToWhitelistFn)

--------------------------------------------------------------------------------
-- | EnableWhitelistFn
--------------------------------------------------------------------------------


type EnableWhitelistFn = Tagged (SProxy "enableWhitelist(bool)") (Tuple1 (Tagged (SProxy "_enabled") Boolean))

enableWhitelist :: TransactionOptions NoPay -> { _enabled :: Boolean } -> Web3 HexString
enableWhitelist x0 r = uncurryFields  r $ enableWhitelist' x0
   where
    enableWhitelist' :: TransactionOptions NoPay -> (Tagged (SProxy "_enabled") Boolean) -> Web3 HexString
    enableWhitelist' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: EnableWhitelistFn)

--------------------------------------------------------------------------------
-- | IsOwnerFn
--------------------------------------------------------------------------------


type IsOwnerFn = Tagged (SProxy "isOwner()") (Tuple0 )

isOwner :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Boolean)
isOwner x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: IsOwnerFn)

--------------------------------------------------------------------------------
-- | IsWhitelistedFn
--------------------------------------------------------------------------------


type IsWhitelistedFn = Tagged (SProxy "isWhitelisted(address)") (Tuple1 (Tagged (SProxy "_address") Address))

isWhitelisted :: TransactionOptions NoPay -> ChainCursor -> { _address :: Address } -> Web3 (Either CallError Boolean)
isWhitelisted x0 cm r = uncurryFields  r $ isWhitelisted' x0 cm
   where
    isWhitelisted' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_address") Address) -> Web3 (Either CallError Boolean)
    isWhitelisted' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 y2) :: IsWhitelistedFn)

--------------------------------------------------------------------------------
-- | OwnerFn
--------------------------------------------------------------------------------


type OwnerFn = Tagged (SProxy "owner()") (Tuple0 )

owner :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Address)
owner x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: OwnerFn)

--------------------------------------------------------------------------------
-- | RemoveFromWhitelistFn
--------------------------------------------------------------------------------


type RemoveFromWhitelistFn = Tagged (SProxy "removeFromWhitelist(address)") (Tuple1 (Tagged (SProxy "_removedAddress") Address))

removeFromWhitelist :: TransactionOptions NoPay -> { _removedAddress :: Address } -> Web3 HexString
removeFromWhitelist x0 r = uncurryFields  r $ removeFromWhitelist' x0
   where
    removeFromWhitelist' :: TransactionOptions NoPay -> (Tagged (SProxy "_removedAddress") Address) -> Web3 HexString
    removeFromWhitelist' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: RemoveFromWhitelistFn)

--------------------------------------------------------------------------------
-- | RenounceOwnershipFn
--------------------------------------------------------------------------------


type RenounceOwnershipFn = Tagged (SProxy "renounceOwnership()") (Tuple0 )

renounceOwnership :: TransactionOptions NoPay -> Web3 HexString
renounceOwnership x0 = sendTx x0 ((tagged $ Tuple0 ) :: RenounceOwnershipFn)

--------------------------------------------------------------------------------
-- | TransferOwnershipFn
--------------------------------------------------------------------------------


type TransferOwnershipFn = Tagged (SProxy "transferOwnership(address)") (Tuple1 (Tagged (SProxy "newOwner") Address))

transferOwnership :: TransactionOptions NoPay -> { newOwner :: Address } -> Web3 HexString
transferOwnership x0 r = uncurryFields  r $ transferOwnership' x0
   where
    transferOwnership' :: TransactionOptions NoPay -> (Tagged (SProxy "newOwner") Address) -> Web3 HexString
    transferOwnership' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: TransferOwnershipFn)