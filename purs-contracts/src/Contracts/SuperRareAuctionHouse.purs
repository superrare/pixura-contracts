--------------------------------------------------------------------------------
-- | SuperRareAuctionHouse
--------------------------------------------------------------------------------

module Contracts.SuperRareAuctionHouse where

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
import Network.Ethereum.Web3.Solidity (BytesN, D2, D3, D5, D6, DOne, Tuple0(..), Tuple1(..), Tuple2(..), Tuple3(..), Tuple4(..), Tuple5(..), UIntN, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Network.Ethereum.Web3.Types.TokenUnit (MinorUnit)
import Partial.Unsafe (unsafePartial)
--------------------------------------------------------------------------------
-- | ConstructorFn
--------------------------------------------------------------------------------


type ConstructorFn = Tagged (SProxy "constructor(address,address)") (Tuple2 (Tagged (SProxy "_iMarketSettings") Address) (Tagged (SProxy "_iERC721CreatorRoyalty") Address))

constructor :: TransactionOptions NoPay -> HexString -> { _iMarketSettings :: Address, _iERC721CreatorRoyalty :: Address } -> Web3 HexString
constructor x0 bc r = uncurryFields  r $ constructor' x0 bc
   where
    constructor' :: TransactionOptions NoPay -> HexString -> (Tagged (SProxy "_iMarketSettings") Address) -> (Tagged (SProxy "_iERC721CreatorRoyalty") Address) -> Web3 HexString
    constructor' y0 bc' y2 y3 = deployContract y0 bc' ((tagged $ Tuple2 y2 y3) :: ConstructorFn)

--------------------------------------------------------------------------------
-- | AuctionBid
--------------------------------------------------------------------------------


newtype AuctionBid = AuctionBid {_contractAddress :: Address,_bidder :: Address,_tokenId :: (UIntN (D2 :& D5 :& DOne D6)),_amount :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeAuctionBid :: Newtype AuctionBid _

instance eventFilterAuctionBid :: EventFilter AuctionBid where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "84fd05b88be83fc762b7bb54dc94b107b11b2bd82db6f2d29cf4fc78929877de"),Nothing,Nothing,Nothing]

instance indexedEventAuctionBid :: IndexedEvent (Tuple3 (Tagged (SProxy "_contractAddress") Address) (Tagged (SProxy "_bidder") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6)))) (Tuple1 (Tagged (SProxy "_amount") (UIntN (D2 :& D5 :& DOne D6)))) AuctionBid where
  isAnonymous _ = false

derive instance genericAuctionBid :: Generic AuctionBid _

instance eventGenericAuctionBidShow :: Show AuctionBid where
  show = genericShow

instance eventGenericAuctionBideq :: Eq AuctionBid where
  eq = genericEq

--------------------------------------------------------------------------------
-- | AuctionCancelBid
--------------------------------------------------------------------------------


newtype AuctionCancelBid = AuctionCancelBid {_bidder :: Address,_contractAddress :: Address,_tokenId :: (UIntN (D2 :& D5 :& DOne D6)),_amount :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeAuctionCancelBid :: Newtype AuctionCancelBid _

instance eventFilterAuctionCancelBid :: EventFilter AuctionCancelBid where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "beef0e0db45c5f09cf59a169dd5ed3ee3efb92eba3d3753520b2d9076ed3e560"),Nothing,Nothing,Nothing]

instance indexedEventAuctionCancelBid :: IndexedEvent (Tuple3 (Tagged (SProxy "_bidder") Address) (Tagged (SProxy "_contractAddress") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6)))) (Tuple1 (Tagged (SProxy "_amount") (UIntN (D2 :& D5 :& DOne D6)))) AuctionCancelBid where
  isAnonymous _ = false

derive instance genericAuctionCancelBid :: Generic AuctionCancelBid _

instance eventGenericAuctionCancelBidShow :: Show AuctionCancelBid where
  show = genericShow

instance eventGenericAuctionCancelBideq :: Eq AuctionCancelBid where
  eq = genericEq

--------------------------------------------------------------------------------
-- | AuctionExtended
--------------------------------------------------------------------------------


newtype AuctionExtended = AuctionExtended {_contractAddress :: Address,_tokenId :: (UIntN (D2 :& D5 :& DOne D6)),_newAuctionLength :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeAuctionExtended :: Newtype AuctionExtended _

instance eventFilterAuctionExtended :: EventFilter AuctionExtended where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "2241aa88c881d125cce08ef2a8f6bfd221ca46e27736a38f9353622a4303c705"),Nothing,Nothing]

instance indexedEventAuctionExtended :: IndexedEvent (Tuple2 (Tagged (SProxy "_contractAddress") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6)))) (Tuple1 (Tagged (SProxy "_newAuctionLength") (UIntN (D2 :& D5 :& DOne D6)))) AuctionExtended where
  isAnonymous _ = false

derive instance genericAuctionExtended :: Generic AuctionExtended _

instance eventGenericAuctionExtendedShow :: Show AuctionExtended where
  show = genericShow

instance eventGenericAuctionExtendedeq :: Eq AuctionExtended where
  eq = genericEq

--------------------------------------------------------------------------------
-- | AuctionSettled
--------------------------------------------------------------------------------


newtype AuctionSettled = AuctionSettled {_contractAddress :: Address,_bidder :: Address,_seller :: Address,_tokenId :: (UIntN (D2 :& D5 :& DOne D6)),_amount :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeAuctionSettled :: Newtype AuctionSettled _

instance eventFilterAuctionSettled :: EventFilter AuctionSettled where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "ea6d16c6bfcad11577aef5cc6728231c9f069ac78393828f8ca96847405902a9"),Nothing,Nothing,Nothing]

instance indexedEventAuctionSettled :: IndexedEvent (Tuple3 (Tagged (SProxy "_contractAddress") Address) (Tagged (SProxy "_bidder") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6)))) (Tuple2 (Tagged (SProxy "_seller") Address) (Tagged (SProxy "_amount") (UIntN (D2 :& D5 :& DOne D6)))) AuctionSettled where
  isAnonymous _ = false

derive instance genericAuctionSettled :: Generic AuctionSettled _

instance eventGenericAuctionSettledShow :: Show AuctionSettled where
  show = genericShow

instance eventGenericAuctionSettledeq :: Eq AuctionSettled where
  eq = genericEq

--------------------------------------------------------------------------------
-- | CancelAuction
--------------------------------------------------------------------------------


newtype CancelAuction = CancelAuction {_contractAddress :: Address,_tokenId :: (UIntN (D2 :& D5 :& DOne D6)),_auctionCreator :: Address}

derive instance newtypeCancelAuction :: Newtype CancelAuction _

instance eventFilterCancelAuction :: EventFilter CancelAuction where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "26d4510b556e779d6507640413e013206e44c8f5d018c7c74ed8926f3f024a9c"),Nothing,Nothing,Nothing]

instance indexedEventCancelAuction :: IndexedEvent (Tuple3 (Tagged (SProxy "_contractAddress") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "_auctionCreator") Address)) (Tuple0 ) CancelAuction where
  isAnonymous _ = false

derive instance genericCancelAuction :: Generic CancelAuction _

instance eventGenericCancelAuctionShow :: Show CancelAuction where
  show = genericShow

instance eventGenericCancelAuctioneq :: Eq CancelAuction where
  eq = genericEq

--------------------------------------------------------------------------------
-- | ColdieAuctionBegun
--------------------------------------------------------------------------------


newtype ColdieAuctionBegun = ColdieAuctionBegun {_bidder :: Address,_contractAddress :: Address,_tokenId :: (UIntN (D2 :& D5 :& DOne D6)),_initialBidAmount :: (UIntN (D2 :& D5 :& DOne D6)),_startingBlock :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeColdieAuctionBegun :: Newtype ColdieAuctionBegun _

instance eventFilterColdieAuctionBegun :: EventFilter ColdieAuctionBegun where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "0686f030efb3419af0300644e95ff05ae08252fd8963da878611d83ee9a67be5"),Nothing,Nothing,Nothing]

instance indexedEventColdieAuctionBegun :: IndexedEvent (Tuple3 (Tagged (SProxy "_bidder") Address) (Tagged (SProxy "_contractAddress") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6)))) (Tuple2 (Tagged (SProxy "_initialBidAmount") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "_startingBlock") (UIntN (D2 :& D5 :& DOne D6)))) ColdieAuctionBegun where
  isAnonymous _ = false

derive instance genericColdieAuctionBegun :: Generic ColdieAuctionBegun _

instance eventGenericColdieAuctionBegunShow :: Show ColdieAuctionBegun where
  show = genericShow

instance eventGenericColdieAuctionBeguneq :: Eq ColdieAuctionBegun where
  eq = genericEq

--------------------------------------------------------------------------------
-- | NewColdieAuction
--------------------------------------------------------------------------------


newtype NewColdieAuction = NewColdieAuction {_contractAddress :: Address,_tokenId :: (UIntN (D2 :& D5 :& DOne D6)),_auctionCreator :: Address,_reservePrice :: (UIntN (D2 :& D5 :& DOne D6)),_lengthOfAuction :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeNewColdieAuction :: Newtype NewColdieAuction _

instance eventFilterNewColdieAuction :: EventFilter NewColdieAuction where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "51d3615735f04b7ff33bfd9f5eb857d899bf9b848d3858cba7db855bc69fb914"),Nothing,Nothing,Nothing]

instance indexedEventNewColdieAuction :: IndexedEvent (Tuple3 (Tagged (SProxy "_contractAddress") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "_auctionCreator") Address)) (Tuple2 (Tagged (SProxy "_reservePrice") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "_lengthOfAuction") (UIntN (D2 :& D5 :& DOne D6)))) NewColdieAuction where
  isAnonymous _ = false

derive instance genericNewColdieAuction :: Generic NewColdieAuction _

instance eventGenericNewColdieAuctionShow :: Show NewColdieAuction where
  show = genericShow

instance eventGenericNewColdieAuctioneq :: Eq NewColdieAuction where
  eq = genericEq

--------------------------------------------------------------------------------
-- | NewScheduledAuction
--------------------------------------------------------------------------------


newtype NewScheduledAuction = NewScheduledAuction {_contractAddress :: Address,_tokenId :: (UIntN (D2 :& D5 :& DOne D6)),_auctionCreator :: Address,_startingBlock :: (UIntN (D2 :& D5 :& DOne D6)),_minimumBid :: (UIntN (D2 :& D5 :& DOne D6)),_lengthOfAuction :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeNewScheduledAuction :: Newtype NewScheduledAuction _

instance eventFilterNewScheduledAuction :: EventFilter NewScheduledAuction where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "97ef537a4a14d8899c80db7b0665dac266da778443e37e073ec2a11ec62bea5b"),Nothing,Nothing,Nothing]

instance indexedEventNewScheduledAuction :: IndexedEvent (Tuple3 (Tagged (SProxy "_contractAddress") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "_auctionCreator") Address)) (Tuple3 (Tagged (SProxy "_startingBlock") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "_minimumBid") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "_lengthOfAuction") (UIntN (D2 :& D5 :& DOne D6)))) NewScheduledAuction where
  isAnonymous _ = false

derive instance genericNewScheduledAuction :: Generic NewScheduledAuction _

instance eventGenericNewScheduledAuctionShow :: Show NewScheduledAuction where
  show = genericShow

instance eventGenericNewScheduledAuctioneq :: Eq NewScheduledAuction where
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
-- | AuctionLengthExtensionFn
--------------------------------------------------------------------------------


type AuctionLengthExtensionFn = Tagged (SProxy "auctionLengthExtension()") (Tuple0 )

auctionLengthExtension :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
auctionLengthExtension x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: AuctionLengthExtensionFn)

--------------------------------------------------------------------------------
-- | BidFn
--------------------------------------------------------------------------------


type BidFn = Tagged (SProxy "bid(address,uint256,uint256)") (Tuple3 (Tagged (SProxy "_contractAddress") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "_amount") (UIntN (D2 :& D5 :& DOne D6))))

bid :: TransactionOptions MinorUnit -> { _contractAddress :: Address, _tokenId :: (UIntN (D2 :& D5 :& DOne D6)), _amount :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
bid x0 r = uncurryFields  r $ bid' x0
   where
    bid' :: TransactionOptions MinorUnit -> (Tagged (SProxy "_contractAddress") Address) -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> (Tagged (SProxy "_amount") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    bid' y0 y1 y2 y3 = sendTx y0 ((tagged $ Tuple3 y1 y2 y3) :: BidFn)

--------------------------------------------------------------------------------
-- | CancelAuctionFn
--------------------------------------------------------------------------------


type CancelAuctionFn = Tagged (SProxy "cancelAuction(address,uint256)") (Tuple2 (Tagged (SProxy "_contractAddress") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

cancelAuction :: TransactionOptions NoPay -> { _contractAddress :: Address, _tokenId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
cancelAuction x0 r = uncurryFields  r $ cancelAuction' x0
   where
    cancelAuction' :: TransactionOptions NoPay -> (Tagged (SProxy "_contractAddress") Address) -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    cancelAuction' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 y1 y2) :: CancelAuctionFn)

--------------------------------------------------------------------------------
-- | CancelBidFn
--------------------------------------------------------------------------------


type CancelBidFn = Tagged (SProxy "cancelBid(address,uint256)") (Tuple2 (Tagged (SProxy "_contractAddress") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

cancelBid :: TransactionOptions NoPay -> { _contractAddress :: Address, _tokenId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
cancelBid x0 r = uncurryFields  r $ cancelBid' x0
   where
    cancelBid' :: TransactionOptions NoPay -> (Tagged (SProxy "_contractAddress") Address) -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    cancelBid' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 y1 y2) :: CancelBidFn)

--------------------------------------------------------------------------------
-- | CreateColdieAuctionFn
--------------------------------------------------------------------------------


type CreateColdieAuctionFn = Tagged (SProxy "createColdieAuction(address,uint256,uint256,uint256)") (Tuple4 (Tagged (SProxy "_contractAddress") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "_reservePrice") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "_lengthOfAuction") (UIntN (D2 :& D5 :& DOne D6))))

createColdieAuction :: TransactionOptions NoPay -> { _contractAddress :: Address, _tokenId :: (UIntN (D2 :& D5 :& DOne D6)), _reservePrice :: (UIntN (D2 :& D5 :& DOne D6)), _lengthOfAuction :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
createColdieAuction x0 r = uncurryFields  r $ createColdieAuction' x0
   where
    createColdieAuction' :: TransactionOptions NoPay -> (Tagged (SProxy "_contractAddress") Address) -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> (Tagged (SProxy "_reservePrice") (UIntN (D2 :& D5 :& DOne D6))) -> (Tagged (SProxy "_lengthOfAuction") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    createColdieAuction' y0 y1 y2 y3 y4 = sendTx y0 ((tagged $ Tuple4 y1 y2 y3 y4) :: CreateColdieAuctionFn)

--------------------------------------------------------------------------------
-- | CreateScheduledAuctionFn
--------------------------------------------------------------------------------


type CreateScheduledAuctionFn = Tagged (SProxy "createScheduledAuction(address,uint256,uint256,uint256,uint256)") (Tuple5 (Tagged (SProxy "_contractAddress") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "_minimumBid") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "_lengthOfAuction") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "_startingBlock") (UIntN (D2 :& D5 :& DOne D6))))

createScheduledAuction :: TransactionOptions NoPay -> { _contractAddress :: Address, _tokenId :: (UIntN (D2 :& D5 :& DOne D6)), _minimumBid :: (UIntN (D2 :& D5 :& DOne D6)), _lengthOfAuction :: (UIntN (D2 :& D5 :& DOne D6)), _startingBlock :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
createScheduledAuction x0 r = uncurryFields  r $ createScheduledAuction' x0
   where
    createScheduledAuction' :: TransactionOptions NoPay -> (Tagged (SProxy "_contractAddress") Address) -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> (Tagged (SProxy "_minimumBid") (UIntN (D2 :& D5 :& DOne D6))) -> (Tagged (SProxy "_lengthOfAuction") (UIntN (D2 :& D5 :& DOne D6))) -> (Tagged (SProxy "_startingBlock") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    createScheduledAuction' y0 y1 y2 y3 y4 y5 = sendTx y0 ((tagged $ Tuple5 y1 y2 y3 y4 y5) :: CreateScheduledAuctionFn)

--------------------------------------------------------------------------------
-- | GetAuctionDetailsFn
--------------------------------------------------------------------------------


type GetAuctionDetailsFn = Tagged (SProxy "getAuctionDetails(address,uint256)") (Tuple2 (Tagged (SProxy "_contractAddress") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

getAuctionDetails :: TransactionOptions NoPay -> ChainCursor -> { _contractAddress :: Address, _tokenId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 (Either CallError (Tuple5 (BytesN (D3 :& DOne D2)) Address (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6))))
getAuctionDetails x0 cm r = uncurryFields  r $ getAuctionDetails' x0 cm
   where
    getAuctionDetails' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_contractAddress") Address) -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 (Either CallError (Tuple5 (BytesN (D3 :& DOne D2)) Address (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6))))
    getAuctionDetails' y0 cm' y2 y3 = call y0 cm' ((tagged $ Tuple2 y2 y3) :: GetAuctionDetailsFn)

--------------------------------------------------------------------------------
-- | GetCurrentBidFn
--------------------------------------------------------------------------------


type GetCurrentBidFn = Tagged (SProxy "getCurrentBid(address,uint256)") (Tuple2 (Tagged (SProxy "_contractAddress") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

getCurrentBid :: TransactionOptions NoPay -> ChainCursor -> { _contractAddress :: Address, _tokenId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 (Either CallError (Tuple2 Address (UIntN (D2 :& D5 :& DOne D6))))
getCurrentBid x0 cm r = uncurryFields  r $ getCurrentBid' x0 cm
   where
    getCurrentBid' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_contractAddress") Address) -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 (Either CallError (Tuple2 Address (UIntN (D2 :& D5 :& DOne D6))))
    getCurrentBid' y0 cm' y2 y3 = call y0 cm' ((tagged $ Tuple2 y2 y3) :: GetCurrentBidFn)

--------------------------------------------------------------------------------
-- | IERC721CreatorRoyaltyFn
--------------------------------------------------------------------------------


type IERC721CreatorRoyaltyFn = Tagged (SProxy "iERC721CreatorRoyalty()") (Tuple0 )

iERC721CreatorRoyalty :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Address)
iERC721CreatorRoyalty x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: IERC721CreatorRoyaltyFn)

--------------------------------------------------------------------------------
-- | IMarketSettingsFn
--------------------------------------------------------------------------------


type IMarketSettingsFn = Tagged (SProxy "iMarketSettings()") (Tuple0 )

iMarketSettings :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Address)
iMarketSettings x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: IMarketSettingsFn)

--------------------------------------------------------------------------------
-- | MaxLengthFn
--------------------------------------------------------------------------------


type MaxLengthFn = Tagged (SProxy "maxLength()") (Tuple0 )

maxLength :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
maxLength x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: MaxLengthFn)

--------------------------------------------------------------------------------
-- | OwnerFn
--------------------------------------------------------------------------------


type OwnerFn = Tagged (SProxy "owner()") (Tuple0 )

owner :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Address)
owner x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: OwnerFn)

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
-- | RenounceOwnershipFn
--------------------------------------------------------------------------------


type RenounceOwnershipFn = Tagged (SProxy "renounceOwnership()") (Tuple0 )

renounceOwnership :: TransactionOptions NoPay -> Web3 HexString
renounceOwnership x0 = sendTx x0 ((tagged $ Tuple0 ) :: RenounceOwnershipFn)

--------------------------------------------------------------------------------
-- | SetAuctionLengthExtensionFn
--------------------------------------------------------------------------------


type SetAuctionLengthExtensionFn = Tagged (SProxy "setAuctionLengthExtension(uint256)") (Tuple1 (Tagged (SProxy "_auctionLengthExtension") (UIntN (D2 :& D5 :& DOne D6))))

setAuctionLengthExtension :: TransactionOptions NoPay -> { _auctionLengthExtension :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
setAuctionLengthExtension x0 r = uncurryFields  r $ setAuctionLengthExtension' x0
   where
    setAuctionLengthExtension' :: TransactionOptions NoPay -> (Tagged (SProxy "_auctionLengthExtension") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    setAuctionLengthExtension' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: SetAuctionLengthExtensionFn)

--------------------------------------------------------------------------------
-- | SetIERC721CreatorRoyaltyFn
--------------------------------------------------------------------------------


type SetIERC721CreatorRoyaltyFn = Tagged (SProxy "setIERC721CreatorRoyalty(address)") (Tuple1 (Tagged (SProxy "_address") Address))

setIERC721CreatorRoyalty :: TransactionOptions NoPay -> { _address :: Address } -> Web3 HexString
setIERC721CreatorRoyalty x0 r = uncurryFields  r $ setIERC721CreatorRoyalty' x0
   where
    setIERC721CreatorRoyalty' :: TransactionOptions NoPay -> (Tagged (SProxy "_address") Address) -> Web3 HexString
    setIERC721CreatorRoyalty' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: SetIERC721CreatorRoyaltyFn)

--------------------------------------------------------------------------------
-- | SetMarketplaceSettingsFn
--------------------------------------------------------------------------------


type SetMarketplaceSettingsFn = Tagged (SProxy "setMarketplaceSettings(address)") (Tuple1 (Tagged (SProxy "_address") Address))

setMarketplaceSettings :: TransactionOptions NoPay -> { _address :: Address } -> Web3 HexString
setMarketplaceSettings x0 r = uncurryFields  r $ setMarketplaceSettings' x0
   where
    setMarketplaceSettings' :: TransactionOptions NoPay -> (Tagged (SProxy "_address") Address) -> Web3 HexString
    setMarketplaceSettings' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: SetMarketplaceSettingsFn)

--------------------------------------------------------------------------------
-- | SetMaxLengthFn
--------------------------------------------------------------------------------


type SetMaxLengthFn = Tagged (SProxy "setMaxLength(uint256)") (Tuple1 (Tagged (SProxy "_maxLength") (UIntN (D2 :& D5 :& DOne D6))))

setMaxLength :: TransactionOptions NoPay -> { _maxLength :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
setMaxLength x0 r = uncurryFields  r $ setMaxLength' x0
   where
    setMaxLength' :: TransactionOptions NoPay -> (Tagged (SProxy "_maxLength") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    setMaxLength' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: SetMaxLengthFn)

--------------------------------------------------------------------------------
-- | SettleAuctionFn
--------------------------------------------------------------------------------


type SettleAuctionFn = Tagged (SProxy "settleAuction(address,uint256)") (Tuple2 (Tagged (SProxy "_contractAddress") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

settleAuction :: TransactionOptions NoPay -> { _contractAddress :: Address, _tokenId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
settleAuction x0 r = uncurryFields  r $ settleAuction' x0
   where
    settleAuction' :: TransactionOptions NoPay -> (Tagged (SProxy "_contractAddress") Address) -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    settleAuction' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 y1 y2) :: SettleAuctionFn)

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
-- | WithdrawPaymentsFn
--------------------------------------------------------------------------------


type WithdrawPaymentsFn = Tagged (SProxy "withdrawPayments(address)") (Tuple1 (Tagged (SProxy "payee") Address))

withdrawPayments :: TransactionOptions NoPay -> { payee :: Address } -> Web3 HexString
withdrawPayments x0 r = uncurryFields  r $ withdrawPayments' x0
   where
    withdrawPayments' :: TransactionOptions NoPay -> (Tagged (SProxy "payee") Address) -> Web3 HexString
    withdrawPayments' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: WithdrawPaymentsFn)