--------------------------------------------------------------------------------
-- | SupeRare
--------------------------------------------------------------------------------

module Contracts.V4.SupeRare where

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
import Network.Ethereum.Web3.Solidity (D2, D5, D6, DOne, Tuple0(..), Tuple1(..), Tuple2(..), Tuple3(..), UIntN, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Network.Ethereum.Web3.Types.TokenUnit (MinorUnit)
import Partial.Unsafe (unsafePartial)
--------------------------------------------------------------------------------
-- | WhitelistCreator
--------------------------------------------------------------------------------


newtype WhitelistCreator = WhitelistCreator {_creator :: Address}

derive instance newtypeWhitelistCreator :: Newtype WhitelistCreator _

instance eventFilterWhitelistCreator :: EventFilter WhitelistCreator where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "55eed0aed3ec6e015b9ad5e984675fe36c0ce3aebdcb70f467670773f19f7f8d"),Nothing]

instance indexedEventWhitelistCreator :: IndexedEvent (Tuple1 (Tagged (SProxy "_creator") Address)) (Tuple0 ) WhitelistCreator where
  isAnonymous _ = false

derive instance genericWhitelistCreator :: Generic WhitelistCreator _

instance eventGenericWhitelistCreatorShow :: Show WhitelistCreator where
  show = genericShow

instance eventGenericWhitelistCreatoreq :: Eq WhitelistCreator where
  eq = genericEq

--------------------------------------------------------------------------------
-- | Bid
--------------------------------------------------------------------------------


newtype Bid = Bid {_bidder :: Address,_amount :: (UIntN (D2 :& D5 :& DOne D6)),_tokenId :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeBid :: Newtype Bid _

instance eventFilterBid :: EventFilter Bid where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "19421268847f42dd61705778018ddfc43bcdce8517e7a630acb12f122c709481"),Nothing,Nothing,Nothing]

instance indexedEventBid :: IndexedEvent (Tuple3 (Tagged (SProxy "_bidder") Address) (Tagged (SProxy "_amount") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6)))) (Tuple0 ) Bid where
  isAnonymous _ = false

derive instance genericBid :: Generic Bid _

instance eventGenericBidShow :: Show Bid where
  show = genericShow

instance eventGenericBideq :: Eq Bid where
  eq = genericEq

--------------------------------------------------------------------------------
-- | AcceptBid
--------------------------------------------------------------------------------


newtype AcceptBid = AcceptBid {_bidder :: Address,_seller :: Address,_amount :: (UIntN (D2 :& D5 :& DOne D6)),_tokenId :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeAcceptBid :: Newtype AcceptBid _

instance eventFilterAcceptBid :: EventFilter AcceptBid where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "d6deddb2e105b46d4644d24aac8c58493a0f107e7973b2fe8d8fa7931a2912be"),Nothing,Nothing,Nothing]

instance indexedEventAcceptBid :: IndexedEvent (Tuple3 (Tagged (SProxy "_bidder") Address) (Tagged (SProxy "_seller") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6)))) (Tuple1 (Tagged (SProxy "_amount") (UIntN (D2 :& D5 :& DOne D6)))) AcceptBid where
  isAnonymous _ = false

derive instance genericAcceptBid :: Generic AcceptBid _

instance eventGenericAcceptBidShow :: Show AcceptBid where
  show = genericShow

instance eventGenericAcceptBideq :: Eq AcceptBid where
  eq = genericEq

--------------------------------------------------------------------------------
-- | CancelBid
--------------------------------------------------------------------------------


newtype CancelBid = CancelBid {_bidder :: Address,_amount :: (UIntN (D2 :& D5 :& DOne D6)),_tokenId :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeCancelBid :: Newtype CancelBid _

instance eventFilterCancelBid :: EventFilter CancelBid where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "09dcebe16a733e22cc47e4959c50d4f21624d9f1815db32c2e439fbbd7b3eda0"),Nothing,Nothing,Nothing]

instance indexedEventCancelBid :: IndexedEvent (Tuple3 (Tagged (SProxy "_bidder") Address) (Tagged (SProxy "_amount") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6)))) (Tuple0 ) CancelBid where
  isAnonymous _ = false

derive instance genericCancelBid :: Generic CancelBid _

instance eventGenericCancelBidShow :: Show CancelBid where
  show = genericShow

instance eventGenericCancelBideq :: Eq CancelBid where
  eq = genericEq

--------------------------------------------------------------------------------
-- | Sold
--------------------------------------------------------------------------------


newtype Sold = Sold {_buyer :: Address,_seller :: Address,_amount :: (UIntN (D2 :& D5 :& DOne D6)),_tokenId :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeSold :: Newtype Sold _

instance eventFilterSold :: EventFilter Sold where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "16dd16959a056953a63cf14bf427881e762e54f03d86b864efea8238dd3b822f"),Nothing,Nothing,Nothing]

instance indexedEventSold :: IndexedEvent (Tuple3 (Tagged (SProxy "_buyer") Address) (Tagged (SProxy "_seller") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6)))) (Tuple1 (Tagged (SProxy "_amount") (UIntN (D2 :& D5 :& DOne D6)))) Sold where
  isAnonymous _ = false

derive instance genericSold :: Generic Sold _

instance eventGenericSoldShow :: Show Sold where
  show = genericShow

instance eventGenericSoldeq :: Eq Sold where
  eq = genericEq

--------------------------------------------------------------------------------
-- | SalePriceSet
--------------------------------------------------------------------------------


newtype SalePriceSet = SalePriceSet {_tokenId :: (UIntN (D2 :& D5 :& DOne D6)),_price :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeSalePriceSet :: Newtype SalePriceSet _

instance eventFilterSalePriceSet :: EventFilter SalePriceSet where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "e23ea816dce6d7f5c0b85cbd597e7c3b97b2453791152c0b94e5e5c5f314d2f0"),Nothing,Nothing]

instance indexedEventSalePriceSet :: IndexedEvent (Tuple2 (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "_price") (UIntN (D2 :& D5 :& DOne D6)))) (Tuple0 ) SalePriceSet where
  isAnonymous _ = false

derive instance genericSalePriceSet :: Generic SalePriceSet _

instance eventGenericSalePriceSetShow :: Show SalePriceSet where
  show = genericShow

instance eventGenericSalePriceSeteq :: Eq SalePriceSet where
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
-- | Transfer
--------------------------------------------------------------------------------


newtype Transfer = Transfer {_from :: Address,_to :: Address,_tokenId :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeTransfer :: Newtype Transfer _

instance eventFilterTransfer :: EventFilter Transfer where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "ddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef"),Nothing,Nothing]

instance indexedEventTransfer :: IndexedEvent (Tuple2 (Tagged (SProxy "_from") Address) (Tagged (SProxy "_to") Address)) (Tuple1 (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6)))) Transfer where
  isAnonymous _ = false

derive instance genericTransfer :: Generic Transfer _

instance eventGenericTransferShow :: Show Transfer where
  show = genericShow

instance eventGenericTransfereq :: Eq Transfer where
  eq = genericEq

--------------------------------------------------------------------------------
-- | Approval
--------------------------------------------------------------------------------


newtype Approval = Approval {_owner :: Address,_approved :: Address,_tokenId :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeApproval :: Newtype Approval _

instance eventFilterApproval :: EventFilter Approval where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "8c5be1e5ebec7d5bd14f71427d1e84f3dd0314c0f7b2291e5b200ac8c7c3b925"),Nothing,Nothing]

instance indexedEventApproval :: IndexedEvent (Tuple2 (Tagged (SProxy "_owner") Address) (Tagged (SProxy "_approved") Address)) (Tuple1 (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6)))) Approval where
  isAnonymous _ = false

derive instance genericApproval :: Generic Approval _

instance eventGenericApprovalShow :: Show Approval where
  show = genericShow

instance eventGenericApprovaleq :: Eq Approval where
  eq = genericEq

--------------------------------------------------------------------------------
-- | AcceptBidFn
--------------------------------------------------------------------------------


type AcceptBidFn = Tagged (SProxy "acceptBid(uint256)") (Tuple1 (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

acceptBid :: TransactionOptions NoPay -> { _tokenId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
acceptBid x0 r = uncurryFields  r $ acceptBid' x0
   where
    acceptBid' :: TransactionOptions NoPay -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    acceptBid' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: AcceptBidFn)

--------------------------------------------------------------------------------
-- | AddNewTokenFn
--------------------------------------------------------------------------------


type AddNewTokenFn = Tagged (SProxy "addNewToken(string)") (Tuple1 (Tagged (SProxy "_uri") String))

addNewToken :: TransactionOptions NoPay -> { _uri :: String } -> Web3 HexString
addNewToken x0 r = uncurryFields  r $ addNewToken' x0
   where
    addNewToken' :: TransactionOptions NoPay -> (Tagged (SProxy "_uri") String) -> Web3 HexString
    addNewToken' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: AddNewTokenFn)

--------------------------------------------------------------------------------
-- | AddNewTokenWithEditionsFn
--------------------------------------------------------------------------------


type AddNewTokenWithEditionsFn = Tagged (SProxy "addNewTokenWithEditions(string,uint256,uint256)") (Tuple3 (Tagged (SProxy "_uri") String) (Tagged (SProxy "_editions") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "_salePrice") (UIntN (D2 :& D5 :& DOne D6))))

addNewTokenWithEditions :: TransactionOptions NoPay -> { _uri :: String, _editions :: (UIntN (D2 :& D5 :& DOne D6)), _salePrice :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
addNewTokenWithEditions x0 r = uncurryFields  r $ addNewTokenWithEditions' x0
   where
    addNewTokenWithEditions' :: TransactionOptions NoPay -> (Tagged (SProxy "_uri") String) -> (Tagged (SProxy "_editions") (UIntN (D2 :& D5 :& DOne D6))) -> (Tagged (SProxy "_salePrice") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    addNewTokenWithEditions' y0 y1 y2 y3 = sendTx y0 ((tagged $ Tuple3 y1 y2 y3) :: AddNewTokenWithEditionsFn)

--------------------------------------------------------------------------------
-- | ApproveFn
--------------------------------------------------------------------------------


type ApproveFn = Tagged (SProxy "approve(address,uint256)") (Tuple2 (Tagged (SProxy "_to") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

approve :: TransactionOptions NoPay -> { _to :: Address, _tokenId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
approve x0 r = uncurryFields  r $ approve' x0
   where
    approve' :: TransactionOptions NoPay -> (Tagged (SProxy "_to") Address) -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    approve' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 y1 y2) :: ApproveFn)

--------------------------------------------------------------------------------
-- | ApprovedForFn
--------------------------------------------------------------------------------


type ApprovedForFn = Tagged (SProxy "approvedFor(uint256)") (Tuple1 (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

approvedFor :: TransactionOptions NoPay -> ChainCursor -> { _tokenId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 (Either CallError Address)
approvedFor x0 cm r = uncurryFields  r $ approvedFor' x0 cm
   where
    approvedFor' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 (Either CallError Address)
    approvedFor' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 y2) :: ApprovedForFn)

--------------------------------------------------------------------------------
-- | BalanceOfFn
--------------------------------------------------------------------------------


type BalanceOfFn = Tagged (SProxy "balanceOf(address)") (Tuple1 (Tagged (SProxy "_owner") Address))

balanceOf :: TransactionOptions NoPay -> ChainCursor -> { _owner :: Address } -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
balanceOf x0 cm r = uncurryFields  r $ balanceOf' x0 cm
   where
    balanceOf' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_owner") Address) -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
    balanceOf' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 y2) :: BalanceOfFn)

--------------------------------------------------------------------------------
-- | BidFn
--------------------------------------------------------------------------------


type BidFn = Tagged (SProxy "bid(uint256)") (Tuple1 (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

bid :: TransactionOptions MinorUnit -> { _tokenId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
bid x0 r = uncurryFields  r $ bid' x0
   where
    bid' :: TransactionOptions MinorUnit -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    bid' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: BidFn)

--------------------------------------------------------------------------------
-- | BuyFn
--------------------------------------------------------------------------------


type BuyFn = Tagged (SProxy "buy(uint256)") (Tuple1 (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

buy :: TransactionOptions MinorUnit -> { _tokenId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
buy x0 r = uncurryFields  r $ buy' x0
   where
    buy' :: TransactionOptions MinorUnit -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    buy' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: BuyFn)

--------------------------------------------------------------------------------
-- | CancelBidFn
--------------------------------------------------------------------------------


type CancelBidFn = Tagged (SProxy "cancelBid(uint256)") (Tuple1 (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

cancelBid :: TransactionOptions NoPay -> { _tokenId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
cancelBid x0 r = uncurryFields  r $ cancelBid' x0
   where
    cancelBid' :: TransactionOptions NoPay -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    cancelBid' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: CancelBidFn)

--------------------------------------------------------------------------------
-- | CreatorOfTokenFn
--------------------------------------------------------------------------------


type CreatorOfTokenFn = Tagged (SProxy "creatorOfToken(uint256)") (Tuple1 (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

creatorOfToken :: TransactionOptions NoPay -> ChainCursor -> { _tokenId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 (Either CallError Address)
creatorOfToken x0 cm r = uncurryFields  r $ creatorOfToken' x0 cm
   where
    creatorOfToken' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 (Either CallError Address)
    creatorOfToken' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 y2) :: CreatorOfTokenFn)

--------------------------------------------------------------------------------
-- | CreatorPercentageFn
--------------------------------------------------------------------------------


type CreatorPercentageFn = Tagged (SProxy "creatorPercentage()") (Tuple0 )

creatorPercentage :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
creatorPercentage x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: CreatorPercentageFn)

--------------------------------------------------------------------------------
-- | CurrentBidDetailsOfTokenFn
--------------------------------------------------------------------------------


type CurrentBidDetailsOfTokenFn = Tagged (SProxy "currentBidDetailsOfToken(uint256)") (Tuple1 (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

currentBidDetailsOfToken :: TransactionOptions NoPay -> ChainCursor -> { _tokenId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 (Either CallError (Tuple2 (UIntN (D2 :& D5 :& DOne D6)) Address))
currentBidDetailsOfToken x0 cm r = uncurryFields  r $ currentBidDetailsOfToken' x0 cm
   where
    currentBidDetailsOfToken' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 (Either CallError (Tuple2 (UIntN (D2 :& D5 :& DOne D6)) Address))
    currentBidDetailsOfToken' y0 cm' y2 = call y0 cm' ((tagged $ Tuple1 y2) :: CurrentBidDetailsOfTokenFn)

--------------------------------------------------------------------------------
-- | IsWhitelistedFn
--------------------------------------------------------------------------------


type IsWhitelistedFn = Tagged (SProxy "isWhitelisted(address)") (Tuple1 (Tagged (SProxy "_creator") Address))

isWhitelisted :: TransactionOptions NoPay -> ChainCursor -> { _creator :: Address } -> Web3 (Either CallError Boolean)
isWhitelisted x0 cm r = uncurryFields  r $ isWhitelisted' x0 cm
   where
    isWhitelisted' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_creator") Address) -> Web3 (Either CallError Boolean)
    isWhitelisted' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 y2) :: IsWhitelistedFn)

--------------------------------------------------------------------------------
-- | MaintainerPercentageFn
--------------------------------------------------------------------------------


type MaintainerPercentageFn = Tagged (SProxy "maintainerPercentage()") (Tuple0 )

maintainerPercentage :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
maintainerPercentage x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: MaintainerPercentageFn)

--------------------------------------------------------------------------------
-- | NameFn
--------------------------------------------------------------------------------


type NameFn = Tagged (SProxy "name()") (Tuple0 )

name :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError String)
name x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: NameFn)

--------------------------------------------------------------------------------
-- | OriginalTokenOfUriFn
--------------------------------------------------------------------------------


type OriginalTokenOfUriFn = Tagged (SProxy "originalTokenOfUri(string)") (Tuple1 (Tagged (SProxy "_uri") String))

originalTokenOfUri :: TransactionOptions NoPay -> ChainCursor -> { _uri :: String } -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
originalTokenOfUri x0 cm r = uncurryFields  r $ originalTokenOfUri' x0 cm
   where
    originalTokenOfUri' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_uri") String) -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
    originalTokenOfUri' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 y2) :: OriginalTokenOfUriFn)

--------------------------------------------------------------------------------
-- | OwnerFn
--------------------------------------------------------------------------------


type OwnerFn = Tagged (SProxy "owner()") (Tuple0 )

owner :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Address)
owner x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: OwnerFn)

--------------------------------------------------------------------------------
-- | OwnerOfFn
--------------------------------------------------------------------------------


type OwnerOfFn = Tagged (SProxy "ownerOf(uint256)") (Tuple1 (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

ownerOf :: TransactionOptions NoPay -> ChainCursor -> { _tokenId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 (Either CallError Address)
ownerOf x0 cm r = uncurryFields  r $ ownerOf' x0 cm
   where
    ownerOf' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 (Either CallError Address)
    ownerOf' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 y2) :: OwnerOfFn)

--------------------------------------------------------------------------------
-- | SalePriceOfTokenFn
--------------------------------------------------------------------------------


type SalePriceOfTokenFn = Tagged (SProxy "salePriceOfToken(uint256)") (Tuple1 (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

salePriceOfToken :: TransactionOptions NoPay -> ChainCursor -> { _tokenId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
salePriceOfToken x0 cm r = uncurryFields  r $ salePriceOfToken' x0 cm
   where
    salePriceOfToken' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
    salePriceOfToken' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 y2) :: SalePriceOfTokenFn)

--------------------------------------------------------------------------------
-- | SetCreatorPercentageFn
--------------------------------------------------------------------------------


type SetCreatorPercentageFn = Tagged (SProxy "setCreatorPercentage(uint256)") (Tuple1 (Tagged (SProxy "_percentage") (UIntN (D2 :& D5 :& DOne D6))))

setCreatorPercentage :: TransactionOptions NoPay -> { _percentage :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
setCreatorPercentage x0 r = uncurryFields  r $ setCreatorPercentage' x0
   where
    setCreatorPercentage' :: TransactionOptions NoPay -> (Tagged (SProxy "_percentage") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    setCreatorPercentage' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: SetCreatorPercentageFn)

--------------------------------------------------------------------------------
-- | SetMaintainerPercentageFn
--------------------------------------------------------------------------------


type SetMaintainerPercentageFn = Tagged (SProxy "setMaintainerPercentage(uint256)") (Tuple1 (Tagged (SProxy "_percentage") (UIntN (D2 :& D5 :& DOne D6))))

setMaintainerPercentage :: TransactionOptions NoPay -> { _percentage :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
setMaintainerPercentage x0 r = uncurryFields  r $ setMaintainerPercentage' x0
   where
    setMaintainerPercentage' :: TransactionOptions NoPay -> (Tagged (SProxy "_percentage") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    setMaintainerPercentage' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: SetMaintainerPercentageFn)

--------------------------------------------------------------------------------
-- | SetSalePriceFn
--------------------------------------------------------------------------------


type SetSalePriceFn = Tagged (SProxy "setSalePrice(uint256,uint256)") (Tuple2 (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "_salePrice") (UIntN (D2 :& D5 :& DOne D6))))

setSalePrice :: TransactionOptions NoPay -> { _tokenId :: (UIntN (D2 :& D5 :& DOne D6)), _salePrice :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
setSalePrice x0 r = uncurryFields  r $ setSalePrice' x0
   where
    setSalePrice' :: TransactionOptions NoPay -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> (Tagged (SProxy "_salePrice") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    setSalePrice' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 y1 y2) :: SetSalePriceFn)

--------------------------------------------------------------------------------
-- | SymbolFn
--------------------------------------------------------------------------------


type SymbolFn = Tagged (SProxy "symbol()") (Tuple0 )

symbol :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError String)
symbol x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: SymbolFn)

--------------------------------------------------------------------------------
-- | TakeOwnershipFn
--------------------------------------------------------------------------------


type TakeOwnershipFn = Tagged (SProxy "takeOwnership(uint256)") (Tuple1 (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

takeOwnership :: TransactionOptions NoPay -> { _tokenId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
takeOwnership x0 r = uncurryFields  r $ takeOwnership' x0
   where
    takeOwnership' :: TransactionOptions NoPay -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    takeOwnership' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: TakeOwnershipFn)

--------------------------------------------------------------------------------
-- | TokenURIFn
--------------------------------------------------------------------------------


type TokenURIFn = Tagged (SProxy "tokenURI(uint256)") (Tuple1 (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

tokenURI :: TransactionOptions NoPay -> ChainCursor -> { _tokenId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 (Either CallError String)
tokenURI x0 cm r = uncurryFields  r $ tokenURI' x0 cm
   where
    tokenURI' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 (Either CallError String)
    tokenURI' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 y2) :: TokenURIFn)

--------------------------------------------------------------------------------
-- | TokensOfFn
--------------------------------------------------------------------------------


type TokensOfFn = Tagged (SProxy "tokensOf(address)") (Tuple1 (Tagged (SProxy "_owner") Address))

tokensOf :: TransactionOptions NoPay -> ChainCursor -> { _owner :: Address } -> Web3 (Either CallError (Array (UIntN (D2 :& D5 :& DOne D6))))
tokensOf x0 cm r = uncurryFields  r $ tokensOf' x0 cm
   where
    tokensOf' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_owner") Address) -> Web3 (Either CallError (Array (UIntN (D2 :& D5 :& DOne D6))))
    tokensOf' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 y2) :: TokensOfFn)

--------------------------------------------------------------------------------
-- | TotalSupplyFn
--------------------------------------------------------------------------------


type TotalSupplyFn = Tagged (SProxy "totalSupply()") (Tuple0 )

totalSupply :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
totalSupply x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: TotalSupplyFn)

--------------------------------------------------------------------------------
-- | TransferFn
--------------------------------------------------------------------------------


type TransferFn = Tagged (SProxy "transfer(address,uint256)") (Tuple2 (Tagged (SProxy "_to") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

transfer :: TransactionOptions NoPay -> { _to :: Address, _tokenId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
transfer x0 r = uncurryFields  r $ transfer' x0
   where
    transfer' :: TransactionOptions NoPay -> (Tagged (SProxy "_to") Address) -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    transfer' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 y1 y2) :: TransferFn)

--------------------------------------------------------------------------------
-- | TransferOwnershipFn
--------------------------------------------------------------------------------


type TransferOwnershipFn = Tagged (SProxy "transferOwnership(address)") (Tuple1 (Tagged (SProxy "newOwner") Address))

transferOwnership :: TransactionOptions NoPay -> { newOwner :: Address } -> Web3 HexString
transferOwnership x0 r = uncurryFields  r $ transferOwnership' x0
   where
    transferOwnership' :: TransactionOptions NoPay -> (Tagged (SProxy "newOwner") Address) -> Web3 HexString
    transferOwnership' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: TransferOwnershipFn)

--------------------------------------------------------------------------------
-- | WhitelistCreatorFn
--------------------------------------------------------------------------------


type WhitelistCreatorFn = Tagged (SProxy "whitelistCreator(address)") (Tuple1 (Tagged (SProxy "_creator") Address))

whitelistCreator :: TransactionOptions NoPay -> { _creator :: Address } -> Web3 HexString
whitelistCreator x0 r = uncurryFields  r $ whitelistCreator' x0
   where
    whitelistCreator' :: TransactionOptions NoPay -> (Tagged (SProxy "_creator") Address) -> Web3 HexString
    whitelistCreator' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: WhitelistCreatorFn)