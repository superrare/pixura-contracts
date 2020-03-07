--------------------------------------------------------------------------------
-- | CryptoPunksMarket
--------------------------------------------------------------------------------

module Contracts.CryptoPunksMarket where

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
import Network.Ethereum.Web3.Solidity (D2, D5, D6, D8, DOne, Tuple0(..), Tuple1(..), Tuple2(..), Tuple3(..), Tuple4, Tuple5, UIntN, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Network.Ethereum.Web3.Types.TokenUnit (MinorUnit)
import Partial.Unsafe (unsafePartial)
--------------------------------------------------------------------------------
-- | ConstructorFn
--------------------------------------------------------------------------------


type ConstructorFn = Tagged (SProxy "constructor()") (Tuple0 )

constructor :: TransactionOptions NoPay -> HexString -> Web3 HexString
constructor x0 bc = deployContract x0 bc ((tagged $ Tuple0 ) :: ConstructorFn)

--------------------------------------------------------------------------------
-- | Assign
--------------------------------------------------------------------------------


newtype Assign = Assign {to :: Address,punkIndex :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeAssign :: Newtype Assign _

instance eventFilterAssign :: EventFilter Assign where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "8a0e37b73a0d9c82e205d4d1a3ff3d0b57ce5f4d7bccf6bac03336dc101cb7ba"),Nothing]

instance indexedEventAssign :: IndexedEvent (Tuple1 (Tagged (SProxy "to") Address)) (Tuple1 (Tagged (SProxy "punkIndex") (UIntN (D2 :& D5 :& DOne D6)))) Assign where
  isAnonymous _ = false

derive instance genericAssign :: Generic Assign _

instance eventGenericAssignShow :: Show Assign where
  show = genericShow

instance eventGenericAssigneq :: Eq Assign where
  eq = genericEq

--------------------------------------------------------------------------------
-- | Transfer
--------------------------------------------------------------------------------


newtype Transfer = Transfer {from :: Address,to :: Address,value :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeTransfer :: Newtype Transfer _

instance eventFilterTransfer :: EventFilter Transfer where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "ddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef"),Nothing,Nothing]

instance indexedEventTransfer :: IndexedEvent (Tuple2 (Tagged (SProxy "from") Address) (Tagged (SProxy "to") Address)) (Tuple1 (Tagged (SProxy "value") (UIntN (D2 :& D5 :& DOne D6)))) Transfer where
  isAnonymous _ = false

derive instance genericTransfer :: Generic Transfer _

instance eventGenericTransferShow :: Show Transfer where
  show = genericShow

instance eventGenericTransfereq :: Eq Transfer where
  eq = genericEq

--------------------------------------------------------------------------------
-- | PunkTransfer
--------------------------------------------------------------------------------


newtype PunkTransfer = PunkTransfer {from :: Address,to :: Address,punkIndex :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypePunkTransfer :: Newtype PunkTransfer _

instance eventFilterPunkTransfer :: EventFilter PunkTransfer where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "05af636b70da6819000c49f85b21fa82081c632069bb626f30932034099107d8"),Nothing,Nothing]

instance indexedEventPunkTransfer :: IndexedEvent (Tuple2 (Tagged (SProxy "from") Address) (Tagged (SProxy "to") Address)) (Tuple1 (Tagged (SProxy "punkIndex") (UIntN (D2 :& D5 :& DOne D6)))) PunkTransfer where
  isAnonymous _ = false

derive instance genericPunkTransfer :: Generic PunkTransfer _

instance eventGenericPunkTransferShow :: Show PunkTransfer where
  show = genericShow

instance eventGenericPunkTransfereq :: Eq PunkTransfer where
  eq = genericEq

--------------------------------------------------------------------------------
-- | PunkOffered
--------------------------------------------------------------------------------


newtype PunkOffered = PunkOffered {punkIndex :: (UIntN (D2 :& D5 :& DOne D6)),minValue :: (UIntN (D2 :& D5 :& DOne D6)),toAddress :: Address}

derive instance newtypePunkOffered :: Newtype PunkOffered _

instance eventFilterPunkOffered :: EventFilter PunkOffered where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "3c7b682d5da98001a9b8cbda6c647d2c63d698a4184fd1d55e2ce7b66f5d21eb"),Nothing,Nothing]

instance indexedEventPunkOffered :: IndexedEvent (Tuple2 (Tagged (SProxy "punkIndex") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "toAddress") Address)) (Tuple1 (Tagged (SProxy "minValue") (UIntN (D2 :& D5 :& DOne D6)))) PunkOffered where
  isAnonymous _ = false

derive instance genericPunkOffered :: Generic PunkOffered _

instance eventGenericPunkOfferedShow :: Show PunkOffered where
  show = genericShow

instance eventGenericPunkOfferedeq :: Eq PunkOffered where
  eq = genericEq

--------------------------------------------------------------------------------
-- | PunkBidEntered
--------------------------------------------------------------------------------


newtype PunkBidEntered = PunkBidEntered {punkIndex :: (UIntN (D2 :& D5 :& DOne D6)),value :: (UIntN (D2 :& D5 :& DOne D6)),fromAddress :: Address}

derive instance newtypePunkBidEntered :: Newtype PunkBidEntered _

instance eventFilterPunkBidEntered :: EventFilter PunkBidEntered where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "5b859394fabae0c1ba88baffe67e751ab5248d2e879028b8c8d6897b0519f56a"),Nothing,Nothing]

instance indexedEventPunkBidEntered :: IndexedEvent (Tuple2 (Tagged (SProxy "punkIndex") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "fromAddress") Address)) (Tuple1 (Tagged (SProxy "value") (UIntN (D2 :& D5 :& DOne D6)))) PunkBidEntered where
  isAnonymous _ = false

derive instance genericPunkBidEntered :: Generic PunkBidEntered _

instance eventGenericPunkBidEnteredShow :: Show PunkBidEntered where
  show = genericShow

instance eventGenericPunkBidEnteredeq :: Eq PunkBidEntered where
  eq = genericEq

--------------------------------------------------------------------------------
-- | PunkBidWithdrawn
--------------------------------------------------------------------------------


newtype PunkBidWithdrawn = PunkBidWithdrawn {punkIndex :: (UIntN (D2 :& D5 :& DOne D6)),value :: (UIntN (D2 :& D5 :& DOne D6)),fromAddress :: Address}

derive instance newtypePunkBidWithdrawn :: Newtype PunkBidWithdrawn _

instance eventFilterPunkBidWithdrawn :: EventFilter PunkBidWithdrawn where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "6f30e1ee4d81dcc7a8a478577f65d2ed2edb120565960ac45fe7c50551c87932"),Nothing,Nothing]

instance indexedEventPunkBidWithdrawn :: IndexedEvent (Tuple2 (Tagged (SProxy "punkIndex") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "fromAddress") Address)) (Tuple1 (Tagged (SProxy "value") (UIntN (D2 :& D5 :& DOne D6)))) PunkBidWithdrawn where
  isAnonymous _ = false

derive instance genericPunkBidWithdrawn :: Generic PunkBidWithdrawn _

instance eventGenericPunkBidWithdrawnShow :: Show PunkBidWithdrawn where
  show = genericShow

instance eventGenericPunkBidWithdrawneq :: Eq PunkBidWithdrawn where
  eq = genericEq

--------------------------------------------------------------------------------
-- | PunkBought
--------------------------------------------------------------------------------


newtype PunkBought = PunkBought {punkIndex :: (UIntN (D2 :& D5 :& DOne D6)),value :: (UIntN (D2 :& D5 :& DOne D6)),fromAddress :: Address,toAddress :: Address}

derive instance newtypePunkBought :: Newtype PunkBought _

instance eventFilterPunkBought :: EventFilter PunkBought where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "58e5d5a525e3b40bc15abaa38b5882678db1ee68befd2f60bafe3a7fd06db9e3"),Nothing,Nothing,Nothing]

instance indexedEventPunkBought :: IndexedEvent (Tuple3 (Tagged (SProxy "punkIndex") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "fromAddress") Address) (Tagged (SProxy "toAddress") Address)) (Tuple1 (Tagged (SProxy "value") (UIntN (D2 :& D5 :& DOne D6)))) PunkBought where
  isAnonymous _ = false

derive instance genericPunkBought :: Generic PunkBought _

instance eventGenericPunkBoughtShow :: Show PunkBought where
  show = genericShow

instance eventGenericPunkBoughteq :: Eq PunkBought where
  eq = genericEq

--------------------------------------------------------------------------------
-- | PunkNoLongerForSale
--------------------------------------------------------------------------------


newtype PunkNoLongerForSale = PunkNoLongerForSale {punkIndex :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypePunkNoLongerForSale :: Newtype PunkNoLongerForSale _

instance eventFilterPunkNoLongerForSale :: EventFilter PunkNoLongerForSale where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "b0e0a660b4e50f26f0b7ce75c24655fc76cc66e3334a54ff410277229fa10bd4"),Nothing]

instance indexedEventPunkNoLongerForSale :: IndexedEvent (Tuple1 (Tagged (SProxy "punkIndex") (UIntN (D2 :& D5 :& DOne D6)))) (Tuple0 ) PunkNoLongerForSale where
  isAnonymous _ = false

derive instance genericPunkNoLongerForSale :: Generic PunkNoLongerForSale _

instance eventGenericPunkNoLongerForSaleShow :: Show PunkNoLongerForSale where
  show = genericShow

instance eventGenericPunkNoLongerForSaleeq :: Eq PunkNoLongerForSale where
  eq = genericEq

--------------------------------------------------------------------------------
-- | AcceptBidForPunkFn
--------------------------------------------------------------------------------


type AcceptBidForPunkFn = Tagged (SProxy "acceptBidForPunk(uint256,uint256)") (Tuple2 (Tagged (SProxy "punkIndex") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "minPrice") (UIntN (D2 :& D5 :& DOne D6))))

acceptBidForPunk :: TransactionOptions NoPay -> { punkIndex :: (UIntN (D2 :& D5 :& DOne D6)), minPrice :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
acceptBidForPunk x0 r = uncurryFields  r $ acceptBidForPunk' x0
   where
    acceptBidForPunk' :: TransactionOptions NoPay -> (Tagged (SProxy "punkIndex") (UIntN (D2 :& D5 :& DOne D6))) -> (Tagged (SProxy "minPrice") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    acceptBidForPunk' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 y1 y2) :: AcceptBidForPunkFn)

--------------------------------------------------------------------------------
-- | AllInitialOwnersAssignedFn
--------------------------------------------------------------------------------


type AllInitialOwnersAssignedFn = Tagged (SProxy "allInitialOwnersAssigned()") (Tuple0 )

allInitialOwnersAssigned :: TransactionOptions NoPay -> Web3 HexString
allInitialOwnersAssigned x0 = sendTx x0 ((tagged $ Tuple0 ) :: AllInitialOwnersAssignedFn)

--------------------------------------------------------------------------------
-- | AllPunksAssignedFn
--------------------------------------------------------------------------------


type AllPunksAssignedFn = Tagged (SProxy "allPunksAssigned()") (Tuple0 )

allPunksAssigned :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Boolean)
allPunksAssigned x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: AllPunksAssignedFn)

--------------------------------------------------------------------------------
-- | BalanceOfFn
--------------------------------------------------------------------------------


type BalanceOfFn = Tagged (SProxy "balanceOf(address)") (Tuple1 Address)

balanceOf :: TransactionOptions NoPay -> ChainCursor -> Address -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
balanceOf x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: BalanceOfFn)

--------------------------------------------------------------------------------
-- | BuyPunkFn
--------------------------------------------------------------------------------


type BuyPunkFn = Tagged (SProxy "buyPunk(uint256)") (Tuple1 (Tagged (SProxy "punkIndex") (UIntN (D2 :& D5 :& DOne D6))))

buyPunk :: TransactionOptions MinorUnit -> { punkIndex :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
buyPunk x0 r = uncurryFields  r $ buyPunk' x0
   where
    buyPunk' :: TransactionOptions MinorUnit -> (Tagged (SProxy "punkIndex") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    buyPunk' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: BuyPunkFn)

--------------------------------------------------------------------------------
-- | DecimalsFn
--------------------------------------------------------------------------------


type DecimalsFn = Tagged (SProxy "decimals()") (Tuple0 )

decimals :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN (DOne D8)))
decimals x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: DecimalsFn)

--------------------------------------------------------------------------------
-- | EnterBidForPunkFn
--------------------------------------------------------------------------------


type EnterBidForPunkFn = Tagged (SProxy "enterBidForPunk(uint256)") (Tuple1 (Tagged (SProxy "punkIndex") (UIntN (D2 :& D5 :& DOne D6))))

enterBidForPunk :: TransactionOptions MinorUnit -> { punkIndex :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
enterBidForPunk x0 r = uncurryFields  r $ enterBidForPunk' x0
   where
    enterBidForPunk' :: TransactionOptions MinorUnit -> (Tagged (SProxy "punkIndex") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    enterBidForPunk' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: EnterBidForPunkFn)

--------------------------------------------------------------------------------
-- | GetPunkFn
--------------------------------------------------------------------------------


type GetPunkFn = Tagged (SProxy "getPunk(uint256)") (Tuple1 (Tagged (SProxy "punkIndex") (UIntN (D2 :& D5 :& DOne D6))))

getPunk :: TransactionOptions NoPay -> { punkIndex :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
getPunk x0 r = uncurryFields  r $ getPunk' x0
   where
    getPunk' :: TransactionOptions NoPay -> (Tagged (SProxy "punkIndex") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    getPunk' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: GetPunkFn)

--------------------------------------------------------------------------------
-- | ImageHashFn
--------------------------------------------------------------------------------


type ImageHashFn = Tagged (SProxy "imageHash()") (Tuple0 )

imageHash :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError String)
imageHash x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: ImageHashFn)

--------------------------------------------------------------------------------
-- | NameFn
--------------------------------------------------------------------------------


type NameFn = Tagged (SProxy "name()") (Tuple0 )

name :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError String)
name x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: NameFn)

--------------------------------------------------------------------------------
-- | NextPunkIndexToAssignFn
--------------------------------------------------------------------------------


type NextPunkIndexToAssignFn = Tagged (SProxy "nextPunkIndexToAssign()") (Tuple0 )

nextPunkIndexToAssign :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
nextPunkIndexToAssign x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: NextPunkIndexToAssignFn)

--------------------------------------------------------------------------------
-- | OfferPunkForSaleFn
--------------------------------------------------------------------------------


type OfferPunkForSaleFn = Tagged (SProxy "offerPunkForSale(uint256,uint256)") (Tuple2 (Tagged (SProxy "punkIndex") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "minSalePriceInWei") (UIntN (D2 :& D5 :& DOne D6))))

offerPunkForSale :: TransactionOptions NoPay -> { punkIndex :: (UIntN (D2 :& D5 :& DOne D6)), minSalePriceInWei :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
offerPunkForSale x0 r = uncurryFields  r $ offerPunkForSale' x0
   where
    offerPunkForSale' :: TransactionOptions NoPay -> (Tagged (SProxy "punkIndex") (UIntN (D2 :& D5 :& DOne D6))) -> (Tagged (SProxy "minSalePriceInWei") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    offerPunkForSale' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 y1 y2) :: OfferPunkForSaleFn)

--------------------------------------------------------------------------------
-- | OfferPunkForSaleToAddressFn
--------------------------------------------------------------------------------


type OfferPunkForSaleToAddressFn = Tagged (SProxy "offerPunkForSaleToAddress(uint256,uint256,address)") (Tuple3 (Tagged (SProxy "punkIndex") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "minSalePriceInWei") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "toAddress") Address))

offerPunkForSaleToAddress :: TransactionOptions NoPay -> { punkIndex :: (UIntN (D2 :& D5 :& DOne D6)), minSalePriceInWei :: (UIntN (D2 :& D5 :& DOne D6)), toAddress :: Address } -> Web3 HexString
offerPunkForSaleToAddress x0 r = uncurryFields  r $ offerPunkForSaleToAddress' x0
   where
    offerPunkForSaleToAddress' :: TransactionOptions NoPay -> (Tagged (SProxy "punkIndex") (UIntN (D2 :& D5 :& DOne D6))) -> (Tagged (SProxy "minSalePriceInWei") (UIntN (D2 :& D5 :& DOne D6))) -> (Tagged (SProxy "toAddress") Address) -> Web3 HexString
    offerPunkForSaleToAddress' y0 y1 y2 y3 = sendTx y0 ((tagged $ Tuple3 y1 y2 y3) :: OfferPunkForSaleToAddressFn)

--------------------------------------------------------------------------------
-- | PendingWithdrawalsFn
--------------------------------------------------------------------------------


type PendingWithdrawalsFn = Tagged (SProxy "pendingWithdrawals(address)") (Tuple1 Address)

pendingWithdrawals :: TransactionOptions NoPay -> ChainCursor -> Address -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
pendingWithdrawals x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: PendingWithdrawalsFn)

--------------------------------------------------------------------------------
-- | PunkBidsFn
--------------------------------------------------------------------------------


type PunkBidsFn = Tagged (SProxy "punkBids(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

punkBids :: TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& DOne D6)) -> Web3 (Either CallError (Tuple4 Boolean (UIntN (D2 :& D5 :& DOne D6)) Address (UIntN (D2 :& D5 :& DOne D6))))
punkBids x0 cm x2 = call x0 cm ((tagged $ Tuple1 x2) :: PunkBidsFn)

--------------------------------------------------------------------------------
-- | PunkIndexToAddressFn
--------------------------------------------------------------------------------


type PunkIndexToAddressFn = Tagged (SProxy "punkIndexToAddress(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

punkIndexToAddress :: TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& DOne D6)) -> Web3 (Either CallError Address)
punkIndexToAddress x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: PunkIndexToAddressFn)

--------------------------------------------------------------------------------
-- | PunkNoLongerForSaleFn
--------------------------------------------------------------------------------


type PunkNoLongerForSaleFn = Tagged (SProxy "punkNoLongerForSale(uint256)") (Tuple1 (Tagged (SProxy "punkIndex") (UIntN (D2 :& D5 :& DOne D6))))

punkNoLongerForSale :: TransactionOptions NoPay -> { punkIndex :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
punkNoLongerForSale x0 r = uncurryFields  r $ punkNoLongerForSale' x0
   where
    punkNoLongerForSale' :: TransactionOptions NoPay -> (Tagged (SProxy "punkIndex") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    punkNoLongerForSale' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: PunkNoLongerForSaleFn)

--------------------------------------------------------------------------------
-- | PunksOfferedForSaleFn
--------------------------------------------------------------------------------


type PunksOfferedForSaleFn = Tagged (SProxy "punksOfferedForSale(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

punksOfferedForSale :: TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& DOne D6)) -> Web3 (Either CallError (Tuple5 Boolean (UIntN (D2 :& D5 :& DOne D6)) Address (UIntN (D2 :& D5 :& DOne D6)) Address))
punksOfferedForSale x0 cm x2 = call x0 cm ((tagged $ Tuple1 x2) :: PunksOfferedForSaleFn)

--------------------------------------------------------------------------------
-- | PunksRemainingToAssignFn
--------------------------------------------------------------------------------


type PunksRemainingToAssignFn = Tagged (SProxy "punksRemainingToAssign()") (Tuple0 )

punksRemainingToAssign :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
punksRemainingToAssign x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: PunksRemainingToAssignFn)

--------------------------------------------------------------------------------
-- | SetInitialOwnerFn
--------------------------------------------------------------------------------


type SetInitialOwnerFn = Tagged (SProxy "setInitialOwner(address,uint256)") (Tuple2 (Tagged (SProxy "to") Address) (Tagged (SProxy "punkIndex") (UIntN (D2 :& D5 :& DOne D6))))

setInitialOwner :: TransactionOptions NoPay -> { to :: Address, punkIndex :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
setInitialOwner x0 r = uncurryFields  r $ setInitialOwner' x0
   where
    setInitialOwner' :: TransactionOptions NoPay -> (Tagged (SProxy "to") Address) -> (Tagged (SProxy "punkIndex") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    setInitialOwner' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 y1 y2) :: SetInitialOwnerFn)

--------------------------------------------------------------------------------
-- | SetInitialOwnersFn
--------------------------------------------------------------------------------


type SetInitialOwnersFn = Tagged (SProxy "setInitialOwners(address[],uint256[])") (Tuple2 (Tagged (SProxy "addresses") (Array Address)) (Tagged (SProxy "indices") (Array (UIntN (D2 :& D5 :& DOne D6)))))

setInitialOwners :: TransactionOptions NoPay -> { addresses :: (Array Address), indices :: (Array (UIntN (D2 :& D5 :& DOne D6))) } -> Web3 HexString
setInitialOwners x0 r = uncurryFields  r $ setInitialOwners' x0
   where
    setInitialOwners' :: TransactionOptions NoPay -> (Tagged (SProxy "addresses") (Array Address)) -> (Tagged (SProxy "indices") (Array (UIntN (D2 :& D5 :& DOne D6)))) -> Web3 HexString
    setInitialOwners' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 y1 y2) :: SetInitialOwnersFn)

--------------------------------------------------------------------------------
-- | StandardFn
--------------------------------------------------------------------------------


type StandardFn = Tagged (SProxy "standard()") (Tuple0 )

standard :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError String)
standard x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: StandardFn)

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
-- | TransferPunkFn
--------------------------------------------------------------------------------


type TransferPunkFn = Tagged (SProxy "transferPunk(address,uint256)") (Tuple2 (Tagged (SProxy "to") Address) (Tagged (SProxy "punkIndex") (UIntN (D2 :& D5 :& DOne D6))))

transferPunk :: TransactionOptions NoPay -> { to :: Address, punkIndex :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
transferPunk x0 r = uncurryFields  r $ transferPunk' x0
   where
    transferPunk' :: TransactionOptions NoPay -> (Tagged (SProxy "to") Address) -> (Tagged (SProxy "punkIndex") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    transferPunk' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 y1 y2) :: TransferPunkFn)

--------------------------------------------------------------------------------
-- | WithdrawFn
--------------------------------------------------------------------------------


type WithdrawFn = Tagged (SProxy "withdraw()") (Tuple0 )

withdraw :: TransactionOptions NoPay -> Web3 HexString
withdraw x0 = sendTx x0 ((tagged $ Tuple0 ) :: WithdrawFn)

--------------------------------------------------------------------------------
-- | WithdrawBidForPunkFn
--------------------------------------------------------------------------------


type WithdrawBidForPunkFn = Tagged (SProxy "withdrawBidForPunk(uint256)") (Tuple1 (Tagged (SProxy "punkIndex") (UIntN (D2 :& D5 :& DOne D6))))

withdrawBidForPunk :: TransactionOptions NoPay -> { punkIndex :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
withdrawBidForPunk x0 r = uncurryFields  r $ withdrawBidForPunk' x0
   where
    withdrawBidForPunk' :: TransactionOptions NoPay -> (Tagged (SProxy "punkIndex") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    withdrawBidForPunk' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: WithdrawBidForPunkFn)