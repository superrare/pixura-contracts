--------------------------------------------------------------------------------
-- | Medianizer
--------------------------------------------------------------------------------

module Contracts.Medianizer where

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
import Network.Ethereum.Web3.Solidity (ByteString, BytesN, D1, D2, D3, D4, D5, D6, D9, DOne, Tuple0(..), Tuple1(..), Tuple2(..), Tuple4, UIntN, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
--------------------------------------------------------------------------------
-- | LogNote
--------------------------------------------------------------------------------


newtype LogNote = LogNote {sig :: (BytesN (DOne D4)),guy :: Address,foo :: (BytesN (D3 :& DOne D2)),bar :: (BytesN (D3 :& DOne D2)),wad :: (UIntN (D2 :& D5 :& DOne D6)),fax :: ByteString}

derive instance newtypeLogNote :: Newtype LogNote _

instance eventFilterLogNote :: EventFilter LogNote where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "644843f351d3fba4abcd60109eaff9f54bac8fb8ccf0bab941009c21df21cf31"),Nothing,Nothing,Nothing,Nothing]

instance indexedEventLogNote :: IndexedEvent (Tuple4 (Tagged (SProxy "sig") (BytesN (DOne D4))) (Tagged (SProxy "guy") Address) (Tagged (SProxy "foo") (BytesN (D3 :& DOne D2))) (Tagged (SProxy "bar") (BytesN (D3 :& DOne D2)))) (Tuple2 (Tagged (SProxy "wad") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "fax") ByteString)) LogNote where
  isAnonymous _ = true

derive instance genericLogNote :: Generic LogNote _

instance eventGenericLogNoteShow :: Show LogNote where
  show = genericShow

instance eventGenericLogNoteeq :: Eq LogNote where
  eq = genericEq

--------------------------------------------------------------------------------
-- | LogSetAuthority
--------------------------------------------------------------------------------


newtype LogSetAuthority = LogSetAuthority {authority :: Address}

derive instance newtypeLogSetAuthority :: Newtype LogSetAuthority _

instance eventFilterLogSetAuthority :: EventFilter LogSetAuthority where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "1abebea81bfa2637f28358c371278fb15ede7ea8dd28d2e03b112ff6d936ada4"),Nothing]

instance indexedEventLogSetAuthority :: IndexedEvent (Tuple1 (Tagged (SProxy "authority") Address)) (Tuple0 ) LogSetAuthority where
  isAnonymous _ = false

derive instance genericLogSetAuthority :: Generic LogSetAuthority _

instance eventGenericLogSetAuthorityShow :: Show LogSetAuthority where
  show = genericShow

instance eventGenericLogSetAuthorityeq :: Eq LogSetAuthority where
  eq = genericEq

--------------------------------------------------------------------------------
-- | LogSetOwner
--------------------------------------------------------------------------------


newtype LogSetOwner = LogSetOwner {owner :: Address}

derive instance newtypeLogSetOwner :: Newtype LogSetOwner _

instance eventFilterLogSetOwner :: EventFilter LogSetOwner where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "ce241d7ca1f669fee44b6fc00b8eba2df3bb514eed0f6f668f8f89096e81ed94"),Nothing]

instance indexedEventLogSetOwner :: IndexedEvent (Tuple1 (Tagged (SProxy "owner") Address)) (Tuple0 ) LogSetOwner where
  isAnonymous _ = false

derive instance genericLogSetOwner :: Generic LogSetOwner _

instance eventGenericLogSetOwnerShow :: Show LogSetOwner where
  show = genericShow

instance eventGenericLogSetOwnereq :: Eq LogSetOwner where
  eq = genericEq

--------------------------------------------------------------------------------
-- | AuthorityFn
--------------------------------------------------------------------------------


type AuthorityFn = Tagged (SProxy "authority()") (Tuple0 )

authority :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Address)
authority x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: AuthorityFn)

--------------------------------------------------------------------------------
-- | ComputeFn
--------------------------------------------------------------------------------


type ComputeFn = Tagged (SProxy "compute()") (Tuple0 )

compute :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (Tuple2 (BytesN (D3 :& DOne D2)) Boolean))
compute x0 cm = call x0 cm ((tagged $ Tuple0 ) :: ComputeFn)

--------------------------------------------------------------------------------
-- | IndexesFn
--------------------------------------------------------------------------------


type IndexesFn = Tagged (SProxy "indexes(address)") (Tuple1 Address)

indexes :: TransactionOptions NoPay -> ChainCursor -> Address -> Web3 (Either CallError (BytesN (D1 :& DOne D2)))
indexes x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: IndexesFn)

--------------------------------------------------------------------------------
-- | MinFn
--------------------------------------------------------------------------------


type MinFn = Tagged (SProxy "min()") (Tuple0 )

min :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN (D9 :& DOne D6)))
min x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: MinFn)

--------------------------------------------------------------------------------
-- | NextFn
--------------------------------------------------------------------------------


type NextFn = Tagged (SProxy "next()") (Tuple0 )

next :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (BytesN (D1 :& DOne D2)))
next x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: NextFn)

--------------------------------------------------------------------------------
-- | OwnerFn
--------------------------------------------------------------------------------


type OwnerFn = Tagged (SProxy "owner()") (Tuple0 )

owner :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Address)
owner x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: OwnerFn)

--------------------------------------------------------------------------------
-- | PeekFn
--------------------------------------------------------------------------------


type PeekFn = Tagged (SProxy "peek()") (Tuple0 )

peek :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (Tuple2 (BytesN (D3 :& DOne D2)) Boolean))
peek x0 cm = call x0 cm ((tagged $ Tuple0 ) :: PeekFn)

--------------------------------------------------------------------------------
-- | Poke0Fn
--------------------------------------------------------------------------------


type Poke0Fn = Tagged (SProxy "poke0()") (Tuple0 )

poke0 :: TransactionOptions NoPay -> Web3 HexString
poke0 x0 = sendTx x0 ((tagged $ Tuple0 ) :: Poke0Fn)

--------------------------------------------------------------------------------
-- | Poke1Fn
--------------------------------------------------------------------------------


type Poke1Fn = Tagged (SProxy "poke1(bytes32)") (Tuple1 (BytesN (D3 :& DOne D2)))

poke1 :: TransactionOptions NoPay -> (BytesN (D3 :& DOne D2)) -> Web3 HexString
poke1 x0 x1 = sendTx x0 ((tagged $ Tuple1 x1) :: Poke1Fn)

--------------------------------------------------------------------------------
-- | ReadFn
--------------------------------------------------------------------------------


type ReadFn = Tagged (SProxy "read()") (Tuple0 )

read :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (BytesN (D3 :& DOne D2)))
read x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: ReadFn)

--------------------------------------------------------------------------------
-- | Set2Fn
--------------------------------------------------------------------------------


type Set2Fn = Tagged (SProxy "set2(bytes12,address)") (Tuple2 (Tagged (SProxy "pos") (BytesN (D1 :& DOne D2))) (Tagged (SProxy "wat") Address))

set2 :: TransactionOptions NoPay -> { pos :: (BytesN (D1 :& DOne D2)), wat :: Address } -> Web3 HexString
set2 x0 r = uncurryFields  r $ set2' x0
   where
    set2' :: TransactionOptions NoPay -> (Tagged (SProxy "pos") (BytesN (D1 :& DOne D2))) -> (Tagged (SProxy "wat") Address) -> Web3 HexString
    set2' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 y1 y2) :: Set2Fn)

--------------------------------------------------------------------------------
-- | Set1Fn
--------------------------------------------------------------------------------


type Set1Fn = Tagged (SProxy "set1(address)") (Tuple1 (Tagged (SProxy "wat") Address))

set1 :: TransactionOptions NoPay -> { wat :: Address } -> Web3 HexString
set1 x0 r = uncurryFields  r $ set1' x0
   where
    set1' :: TransactionOptions NoPay -> (Tagged (SProxy "wat") Address) -> Web3 HexString
    set1' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: Set1Fn)

--------------------------------------------------------------------------------
-- | SetAuthorityFn
--------------------------------------------------------------------------------


type SetAuthorityFn = Tagged (SProxy "setAuthority(address)") (Tuple1 (Tagged (SProxy "authority_") Address))

setAuthority :: TransactionOptions NoPay -> { authority_ :: Address } -> Web3 HexString
setAuthority x0 r = uncurryFields  r $ setAuthority' x0
   where
    setAuthority' :: TransactionOptions NoPay -> (Tagged (SProxy "authority_") Address) -> Web3 HexString
    setAuthority' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: SetAuthorityFn)

--------------------------------------------------------------------------------
-- | SetMinFn
--------------------------------------------------------------------------------


type SetMinFn = Tagged (SProxy "setMin(uint96)") (Tuple1 (Tagged (SProxy "min_") (UIntN (D9 :& DOne D6))))

setMin :: TransactionOptions NoPay -> { min_ :: (UIntN (D9 :& DOne D6)) } -> Web3 HexString
setMin x0 r = uncurryFields  r $ setMin' x0
   where
    setMin' :: TransactionOptions NoPay -> (Tagged (SProxy "min_") (UIntN (D9 :& DOne D6))) -> Web3 HexString
    setMin' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: SetMinFn)

--------------------------------------------------------------------------------
-- | SetNextFn
--------------------------------------------------------------------------------


type SetNextFn = Tagged (SProxy "setNext(bytes12)") (Tuple1 (Tagged (SProxy "next_") (BytesN (D1 :& DOne D2))))

setNext :: TransactionOptions NoPay -> { next_ :: (BytesN (D1 :& DOne D2)) } -> Web3 HexString
setNext x0 r = uncurryFields  r $ setNext' x0
   where
    setNext' :: TransactionOptions NoPay -> (Tagged (SProxy "next_") (BytesN (D1 :& DOne D2))) -> Web3 HexString
    setNext' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: SetNextFn)

--------------------------------------------------------------------------------
-- | SetOwnerFn
--------------------------------------------------------------------------------


type SetOwnerFn = Tagged (SProxy "setOwner(address)") (Tuple1 (Tagged (SProxy "owner_") Address))

setOwner :: TransactionOptions NoPay -> { owner_ :: Address } -> Web3 HexString
setOwner x0 r = uncurryFields  r $ setOwner' x0
   where
    setOwner' :: TransactionOptions NoPay -> (Tagged (SProxy "owner_") Address) -> Web3 HexString
    setOwner' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: SetOwnerFn)

--------------------------------------------------------------------------------
-- | Unset1Fn
--------------------------------------------------------------------------------


type Unset1Fn = Tagged (SProxy "unset1(bytes12)") (Tuple1 (Tagged (SProxy "pos") (BytesN (D1 :& DOne D2))))

unset1 :: TransactionOptions NoPay -> { pos :: (BytesN (D1 :& DOne D2)) } -> Web3 HexString
unset1 x0 r = uncurryFields  r $ unset1' x0
   where
    unset1' :: TransactionOptions NoPay -> (Tagged (SProxy "pos") (BytesN (D1 :& DOne D2))) -> Web3 HexString
    unset1' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: Unset1Fn)

--------------------------------------------------------------------------------
-- | Unset1Fn
--------------------------------------------------------------------------------


type Unset1Fn = Tagged (SProxy "unset1(address)") (Tuple1 (Tagged (SProxy "wat") Address))

unset1 :: TransactionOptions NoPay -> { wat :: Address } -> Web3 HexString
unset1 x0 r = uncurryFields  r $ unset1' x0
   where
    unset1' :: TransactionOptions NoPay -> (Tagged (SProxy "wat") Address) -> Web3 HexString
    unset1' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: Unset1Fn)

--------------------------------------------------------------------------------
-- | ValuesFn
--------------------------------------------------------------------------------


type ValuesFn = Tagged (SProxy "values(bytes12)") (Tuple1 (BytesN (D1 :& DOne D2)))

values :: TransactionOptions NoPay -> ChainCursor -> (BytesN (D1 :& DOne D2)) -> Web3 (Either CallError Address)
values x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: ValuesFn)

--------------------------------------------------------------------------------
-- | VoidFn
--------------------------------------------------------------------------------


type VoidFn = Tagged (SProxy "void()") (Tuple0 )

void :: TransactionOptions NoPay -> Web3 HexString
void x0 = sendTx x0 ((tagged $ Tuple0 ) :: VoidFn)