--------------------------------------------------------------------------------
-- | SuperRareMarketAuctionV2
--------------------------------------------------------------------------------

module Contracts.V5.SuperRareMarketAuctionV2 where

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
-- | AcceptBid
--------------------------------------------------------------------------------


newtype AcceptBid = AcceptBid {_originContract :: Address,_bidder :: Address,_seller :: Address,_amount :: (UIntN (D2 :& D5 :& DOne D6)),_tokenId :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeAcceptBid :: Newtype AcceptBid _

instance eventFilterAcceptBid :: EventFilter AcceptBid where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "2a9d06eec42acd217a17785dbec90b8b4f01a93ecd8c127edd36bfccf239f8b6"),Nothing,Nothing,Nothing]

instance indexedEventAcceptBid :: IndexedEvent (Tuple3 (Tagged (SProxy "_originContract") Address) (Tagged (SProxy "_bidder") Address) (Tagged (SProxy "_seller") Address)) (Tuple2 (Tagged (SProxy "_amount") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6)))) AcceptBid where
  isAnonymous _ = false

derive instance genericAcceptBid :: Generic AcceptBid _

instance eventGenericAcceptBidShow :: Show AcceptBid where
  show = genericShow

instance eventGenericAcceptBideq :: Eq AcceptBid where
  eq = genericEq

--------------------------------------------------------------------------------
-- | Bid
--------------------------------------------------------------------------------


newtype Bid = Bid {_originContract :: Address,_bidder :: Address,_amount :: (UIntN (D2 :& D5 :& DOne D6)),_tokenId :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeBid :: Newtype Bid _

instance eventFilterBid :: EventFilter Bid where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "d21fbaad97462831ad0c216f300fefb33a10b03bb18bb70ed668562e88d15d53"),Nothing,Nothing]

instance indexedEventBid :: IndexedEvent (Tuple2 (Tagged (SProxy "_originContract") Address) (Tagged (SProxy "_bidder") Address)) (Tuple2 (Tagged (SProxy "_amount") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6)))) Bid where
  isAnonymous _ = false

derive instance genericBid :: Generic Bid _

instance eventGenericBidShow :: Show Bid where
  show = genericShow

instance eventGenericBideq :: Eq Bid where
  eq = genericEq

--------------------------------------------------------------------------------
-- | CancelBid
--------------------------------------------------------------------------------


newtype CancelBid = CancelBid {_originContract :: Address,_bidder :: Address,_amount :: (UIntN (D2 :& D5 :& DOne D6)),_tokenId :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeCancelBid :: Newtype CancelBid _

instance eventFilterCancelBid :: EventFilter CancelBid where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "99a3761c98d7a0c3980cbeb3d8009b315a463f8020b43ca1e6901611b06547f9"),Nothing,Nothing]

instance indexedEventCancelBid :: IndexedEvent (Tuple2 (Tagged (SProxy "_originContract") Address) (Tagged (SProxy "_bidder") Address)) (Tuple2 (Tagged (SProxy "_amount") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6)))) CancelBid where
  isAnonymous _ = false

derive instance genericCancelBid :: Generic CancelBid _

instance eventGenericCancelBidShow :: Show CancelBid where
  show = genericShow

instance eventGenericCancelBideq :: Eq CancelBid where
  eq = genericEq

--------------------------------------------------------------------------------
-- | MarketplaceFeeSet
--------------------------------------------------------------------------------


newtype MarketplaceFeeSet = MarketplaceFeeSet {_percentage :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeMarketplaceFeeSet :: Newtype MarketplaceFeeSet _

instance eventFilterMarketplaceFeeSet :: EventFilter MarketplaceFeeSet where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "d81769cfbd70021010dde0ce784c926888ec237876c2d51c85b90f11065bf542"),Nothing]

instance indexedEventMarketplaceFeeSet :: IndexedEvent (Tuple1 (Tagged (SProxy "_percentage") (UIntN (D2 :& D5 :& DOne D6)))) (Tuple0 ) MarketplaceFeeSet where
  isAnonymous _ = false

derive instance genericMarketplaceFeeSet :: Generic MarketplaceFeeSet _

instance eventGenericMarketplaceFeeSetShow :: Show MarketplaceFeeSet where
  show = genericShow

instance eventGenericMarketplaceFeeSeteq :: Eq MarketplaceFeeSet where
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
-- | PrimarySalePercentageSet
--------------------------------------------------------------------------------


newtype PrimarySalePercentageSet = PrimarySalePercentageSet {_originContract :: Address,_percentage :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypePrimarySalePercentageSet :: Newtype PrimarySalePercentageSet _

instance eventFilterPrimarySalePercentageSet :: EventFilter PrimarySalePercentageSet where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "acd8fb6367088ecead1c71d4949c7b16e86cbcfa22f5818cfbd154e80224f46b"),Nothing]

instance indexedEventPrimarySalePercentageSet :: IndexedEvent (Tuple1 (Tagged (SProxy "_originContract") Address)) (Tuple1 (Tagged (SProxy "_percentage") (UIntN (D2 :& D5 :& DOne D6)))) PrimarySalePercentageSet where
  isAnonymous _ = false

derive instance genericPrimarySalePercentageSet :: Generic PrimarySalePercentageSet _

instance eventGenericPrimarySalePercentageSetShow :: Show PrimarySalePercentageSet where
  show = genericShow

instance eventGenericPrimarySalePercentageSeteq :: Eq PrimarySalePercentageSet where
  eq = genericEq

--------------------------------------------------------------------------------
-- | RoyaltySettingsSet
--------------------------------------------------------------------------------


newtype RoyaltySettingsSet = RoyaltySettingsSet {_originContract :: Address,_erc721CreatorContract :: Address,_percentage :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeRoyaltySettingsSet :: Newtype RoyaltySettingsSet _

instance eventFilterRoyaltySettingsSet :: EventFilter RoyaltySettingsSet where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "f0e6a394adf6e0ec60cc7b657f02a07a301da7da4d8f7f0882a15e1a8669b8be"),Nothing,Nothing]

instance indexedEventRoyaltySettingsSet :: IndexedEvent (Tuple2 (Tagged (SProxy "_originContract") Address) (Tagged (SProxy "_erc721CreatorContract") Address)) (Tuple1 (Tagged (SProxy "_percentage") (UIntN (D2 :& D5 :& DOne D6)))) RoyaltySettingsSet where
  isAnonymous _ = false

derive instance genericRoyaltySettingsSet :: Generic RoyaltySettingsSet _

instance eventGenericRoyaltySettingsSetShow :: Show RoyaltySettingsSet where
  show = genericShow

instance eventGenericRoyaltySettingsSeteq :: Eq RoyaltySettingsSet where
  eq = genericEq

--------------------------------------------------------------------------------
-- | SetSalePrice
--------------------------------------------------------------------------------


newtype SetSalePrice = SetSalePrice {_originContract :: Address,_amount :: (UIntN (D2 :& D5 :& DOne D6)),_tokenId :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeSetSalePrice :: Newtype SetSalePrice _

instance eventFilterSetSalePrice :: EventFilter SetSalePrice where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "b0b0e4adf2724af8f1646eae3a16f45d696c9334594729d09bf192da1f783871"),Nothing]

instance indexedEventSetSalePrice :: IndexedEvent (Tuple1 (Tagged (SProxy "_originContract") Address)) (Tuple2 (Tagged (SProxy "_amount") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6)))) SetSalePrice where
  isAnonymous _ = false

derive instance genericSetSalePrice :: Generic SetSalePrice _

instance eventGenericSetSalePriceShow :: Show SetSalePrice where
  show = genericShow

instance eventGenericSetSalePriceeq :: Eq SetSalePrice where
  eq = genericEq

--------------------------------------------------------------------------------
-- | Sold
--------------------------------------------------------------------------------


newtype Sold = Sold {_originContract :: Address,_buyer :: Address,_seller :: Address,_amount :: (UIntN (D2 :& D5 :& DOne D6)),_tokenId :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeSold :: Newtype Sold _

instance eventFilterSold :: EventFilter Sold where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "5764dbcef91eb6f946584f4ea671217c686fa7e858ce4f9f42d08422b86556a9"),Nothing,Nothing,Nothing]

instance indexedEventSold :: IndexedEvent (Tuple3 (Tagged (SProxy "_originContract") Address) (Tagged (SProxy "_buyer") Address) (Tagged (SProxy "_seller") Address)) (Tuple2 (Tagged (SProxy "_amount") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6)))) Sold where
  isAnonymous _ = false

derive instance genericSold :: Generic Sold _

instance eventGenericSoldShow :: Show Sold where
  show = genericShow

instance eventGenericSoldeq :: Eq Sold where
  eq = genericEq

--------------------------------------------------------------------------------
-- | AcceptBidFn
--------------------------------------------------------------------------------


type AcceptBidFn = Tagged (SProxy "acceptBid(address,uint256)") (Tuple2 (Tagged (SProxy "_originContract") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

acceptBid :: TransactionOptions NoPay -> { _originContract :: Address, _tokenId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
acceptBid x0 r = uncurryFields  r $ acceptBid' x0
   where
    acceptBid' :: TransactionOptions NoPay -> (Tagged (SProxy "_originContract") Address) -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    acceptBid' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 y1 y2) :: AcceptBidFn)

--------------------------------------------------------------------------------
-- | BidFn
--------------------------------------------------------------------------------


type BidFn = Tagged (SProxy "bid(uint256,address,uint256)") (Tuple3 (Tagged (SProxy "_newBidAmount") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "_originContract") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

bid :: TransactionOptions MinorUnit -> { _newBidAmount :: (UIntN (D2 :& D5 :& DOne D6)), _originContract :: Address, _tokenId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
bid x0 r = uncurryFields  r $ bid' x0
   where
    bid' :: TransactionOptions MinorUnit -> (Tagged (SProxy "_newBidAmount") (UIntN (D2 :& D5 :& DOne D6))) -> (Tagged (SProxy "_originContract") Address) -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    bid' y0 y1 y2 y3 = sendTx y0 ((tagged $ Tuple3 y1 y2 y3) :: BidFn)

--------------------------------------------------------------------------------
-- | BuyFn
--------------------------------------------------------------------------------


type BuyFn = Tagged (SProxy "buy(address,uint256)") (Tuple2 (Tagged (SProxy "_originContract") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

buy :: TransactionOptions MinorUnit -> { _originContract :: Address, _tokenId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
buy x0 r = uncurryFields  r $ buy' x0
   where
    buy' :: TransactionOptions MinorUnit -> (Tagged (SProxy "_originContract") Address) -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    buy' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 y1 y2) :: BuyFn)

--------------------------------------------------------------------------------
-- | CancelBidFn
--------------------------------------------------------------------------------


type CancelBidFn = Tagged (SProxy "cancelBid(address,uint256)") (Tuple2 (Tagged (SProxy "_originContract") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

cancelBid :: TransactionOptions NoPay -> { _originContract :: Address, _tokenId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
cancelBid x0 r = uncurryFields  r $ cancelBid' x0
   where
    cancelBid' :: TransactionOptions NoPay -> (Tagged (SProxy "_originContract") Address) -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    cancelBid' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 y1 y2) :: CancelBidFn)

--------------------------------------------------------------------------------
-- | CurrentBidDetailsOfTokenFn
--------------------------------------------------------------------------------


type CurrentBidDetailsOfTokenFn = Tagged (SProxy "currentBidDetailsOfToken(address,uint256)") (Tuple2 (Tagged (SProxy "_originContract") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

currentBidDetailsOfToken :: TransactionOptions NoPay -> ChainCursor -> { _originContract :: Address, _tokenId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 (Either CallError (Tuple2 (UIntN (D2 :& D5 :& DOne D6)) Address))
currentBidDetailsOfToken x0 cm r = uncurryFields  r $ currentBidDetailsOfToken' x0 cm
   where
    currentBidDetailsOfToken' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_originContract") Address) -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 (Either CallError (Tuple2 (UIntN (D2 :& D5 :& DOne D6)) Address))
    currentBidDetailsOfToken' y0 cm' y2 y3 = call y0 cm' ((tagged $ Tuple2 y2 y3) :: CurrentBidDetailsOfTokenFn)

--------------------------------------------------------------------------------
-- | GetERC721ContractPrimarySaleFeeFn
--------------------------------------------------------------------------------


type GetERC721ContractPrimarySaleFeeFn = Tagged (SProxy "getERC721ContractPrimarySaleFee(address)") (Tuple1 (Tagged (SProxy "_originContract") Address))

getERC721ContractPrimarySaleFee :: TransactionOptions NoPay -> ChainCursor -> { _originContract :: Address } -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getERC721ContractPrimarySaleFee x0 cm r = uncurryFields  r $ getERC721ContractPrimarySaleFee' x0 cm
   where
    getERC721ContractPrimarySaleFee' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_originContract") Address) -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
    getERC721ContractPrimarySaleFee' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 y2) :: GetERC721ContractPrimarySaleFeeFn)

--------------------------------------------------------------------------------
-- | GetERC721ContractRoyaltySettingsFn
--------------------------------------------------------------------------------


type GetERC721ContractRoyaltySettingsFn = Tagged (SProxy "getERC721ContractRoyaltySettings(address)") (Tuple1 (Tagged (SProxy "_originContract") Address))

getERC721ContractRoyaltySettings :: TransactionOptions NoPay -> ChainCursor -> { _originContract :: Address } -> Web3 (Either CallError (Tuple2 Address (UIntN (D2 :& D5 :& DOne D6))))
getERC721ContractRoyaltySettings x0 cm r = uncurryFields  r $ getERC721ContractRoyaltySettings' x0 cm
   where
    getERC721ContractRoyaltySettings' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_originContract") Address) -> Web3 (Either CallError (Tuple2 Address (UIntN (D2 :& D5 :& DOne D6))))
    getERC721ContractRoyaltySettings' y0 cm' y2 = call y0 cm' ((tagged $ Tuple1 y2) :: GetERC721ContractRoyaltySettingsFn)

--------------------------------------------------------------------------------
-- | HasTokenBeenSoldFn
--------------------------------------------------------------------------------


type HasTokenBeenSoldFn = Tagged (SProxy "hasTokenBeenSold(address,uint256)") (Tuple2 (Tagged (SProxy "_originContract") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

hasTokenBeenSold :: TransactionOptions NoPay -> ChainCursor -> { _originContract :: Address, _tokenId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 (Either CallError Boolean)
hasTokenBeenSold x0 cm r = uncurryFields  r $ hasTokenBeenSold' x0 cm
   where
    hasTokenBeenSold' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_originContract") Address) -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 (Either CallError Boolean)
    hasTokenBeenSold' y0 cm' y2 y3 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple2 y2 y3) :: HasTokenBeenSoldFn)

--------------------------------------------------------------------------------
-- | IsOwnerFn
--------------------------------------------------------------------------------


type IsOwnerFn = Tagged (SProxy "isOwner()") (Tuple0 )

isOwner :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Boolean)
isOwner x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: IsOwnerFn)

--------------------------------------------------------------------------------
-- | MarkTokensAsSoldFn
--------------------------------------------------------------------------------


type MarkTokensAsSoldFn = Tagged (SProxy "markTokensAsSold(address,uint256[])") (Tuple2 (Tagged (SProxy "_originContract") Address) (Tagged (SProxy "_tokenIds") (Array (UIntN (D2 :& D5 :& DOne D6)))))

markTokensAsSold :: TransactionOptions NoPay -> { _originContract :: Address, _tokenIds :: (Array (UIntN (D2 :& D5 :& DOne D6))) } -> Web3 HexString
markTokensAsSold x0 r = uncurryFields  r $ markTokensAsSold' x0
   where
    markTokensAsSold' :: TransactionOptions NoPay -> (Tagged (SProxy "_originContract") Address) -> (Tagged (SProxy "_tokenIds") (Array (UIntN (D2 :& D5 :& DOne D6)))) -> Web3 HexString
    markTokensAsSold' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 y1 y2) :: MarkTokensAsSoldFn)

--------------------------------------------------------------------------------
-- | MarketplaceFeeFn
--------------------------------------------------------------------------------


type MarketplaceFeeFn = Tagged (SProxy "marketplaceFee()") (Tuple0 )

marketplaceFee :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
marketplaceFee x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: MarketplaceFeeFn)

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
-- | SafeAcceptBidFn
--------------------------------------------------------------------------------


type SafeAcceptBidFn = Tagged (SProxy "safeAcceptBid(address,uint256,uint256)") (Tuple3 (Tagged (SProxy "_originContract") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "_amount") (UIntN (D2 :& D5 :& DOne D6))))

safeAcceptBid :: TransactionOptions NoPay -> { _originContract :: Address, _tokenId :: (UIntN (D2 :& D5 :& DOne D6)), _amount :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
safeAcceptBid x0 r = uncurryFields  r $ safeAcceptBid' x0
   where
    safeAcceptBid' :: TransactionOptions NoPay -> (Tagged (SProxy "_originContract") Address) -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> (Tagged (SProxy "_amount") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    safeAcceptBid' y0 y1 y2 y3 = sendTx y0 ((tagged $ Tuple3 y1 y2 y3) :: SafeAcceptBidFn)

--------------------------------------------------------------------------------
-- | SafeBuyFn
--------------------------------------------------------------------------------


type SafeBuyFn = Tagged (SProxy "safeBuy(address,uint256,uint256)") (Tuple3 (Tagged (SProxy "_originContract") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "_amount") (UIntN (D2 :& D5 :& DOne D6))))

safeBuy :: TransactionOptions MinorUnit -> { _originContract :: Address, _tokenId :: (UIntN (D2 :& D5 :& DOne D6)), _amount :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
safeBuy x0 r = uncurryFields  r $ safeBuy' x0
   where
    safeBuy' :: TransactionOptions MinorUnit -> (Tagged (SProxy "_originContract") Address) -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> (Tagged (SProxy "_amount") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    safeBuy' y0 y1 y2 y3 = sendTx y0 ((tagged $ Tuple3 y1 y2 y3) :: SafeBuyFn)

--------------------------------------------------------------------------------
-- | SetERC721ContractPrimarySaleFeeFn
--------------------------------------------------------------------------------


type SetERC721ContractPrimarySaleFeeFn = Tagged (SProxy "setERC721ContractPrimarySaleFee(address,uint256)") (Tuple2 (Tagged (SProxy "_originContract") Address) (Tagged (SProxy "_percentage") (UIntN (D2 :& D5 :& DOne D6))))

setERC721ContractPrimarySaleFee :: TransactionOptions NoPay -> { _originContract :: Address, _percentage :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
setERC721ContractPrimarySaleFee x0 r = uncurryFields  r $ setERC721ContractPrimarySaleFee' x0
   where
    setERC721ContractPrimarySaleFee' :: TransactionOptions NoPay -> (Tagged (SProxy "_originContract") Address) -> (Tagged (SProxy "_percentage") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    setERC721ContractPrimarySaleFee' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 y1 y2) :: SetERC721ContractPrimarySaleFeeFn)

--------------------------------------------------------------------------------
-- | SetERC721ContractRoyaltySettingsFn
--------------------------------------------------------------------------------


type SetERC721ContractRoyaltySettingsFn = Tagged (SProxy "setERC721ContractRoyaltySettings(address,address,uint256)") (Tuple3 (Tagged (SProxy "_originContract") Address) (Tagged (SProxy "_erc721CreatorContract") Address) (Tagged (SProxy "_percentage") (UIntN (D2 :& D5 :& DOne D6))))

setERC721ContractRoyaltySettings :: TransactionOptions NoPay -> { _originContract :: Address, _erc721CreatorContract :: Address, _percentage :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
setERC721ContractRoyaltySettings x0 r = uncurryFields  r $ setERC721ContractRoyaltySettings' x0
   where
    setERC721ContractRoyaltySettings' :: TransactionOptions NoPay -> (Tagged (SProxy "_originContract") Address) -> (Tagged (SProxy "_erc721CreatorContract") Address) -> (Tagged (SProxy "_percentage") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    setERC721ContractRoyaltySettings' y0 y1 y2 y3 = sendTx y0 ((tagged $ Tuple3 y1 y2 y3) :: SetERC721ContractRoyaltySettingsFn)

--------------------------------------------------------------------------------
-- | SetMarketplaceFeeFn
--------------------------------------------------------------------------------


type SetMarketplaceFeeFn = Tagged (SProxy "setMarketplaceFee(uint256)") (Tuple1 (Tagged (SProxy "_percentage") (UIntN (D2 :& D5 :& DOne D6))))

setMarketplaceFee :: TransactionOptions NoPay -> { _percentage :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
setMarketplaceFee x0 r = uncurryFields  r $ setMarketplaceFee' x0
   where
    setMarketplaceFee' :: TransactionOptions NoPay -> (Tagged (SProxy "_percentage") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    setMarketplaceFee' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: SetMarketplaceFeeFn)

--------------------------------------------------------------------------------
-- | SetSalePriceFn
--------------------------------------------------------------------------------


type SetSalePriceFn = Tagged (SProxy "setSalePrice(address,uint256,uint256)") (Tuple3 (Tagged (SProxy "_originContract") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "_amount") (UIntN (D2 :& D5 :& DOne D6))))

setSalePrice :: TransactionOptions NoPay -> { _originContract :: Address, _tokenId :: (UIntN (D2 :& D5 :& DOne D6)), _amount :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
setSalePrice x0 r = uncurryFields  r $ setSalePrice' x0
   where
    setSalePrice' :: TransactionOptions NoPay -> (Tagged (SProxy "_originContract") Address) -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> (Tagged (SProxy "_amount") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    setSalePrice' y0 y1 y2 y3 = sendTx y0 ((tagged $ Tuple3 y1 y2 y3) :: SetSalePriceFn)

--------------------------------------------------------------------------------
-- | TokenPriceFn
--------------------------------------------------------------------------------


type TokenPriceFn = Tagged (SProxy "tokenPrice(address,uint256)") (Tuple2 (Tagged (SProxy "_originContract") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

tokenPrice :: TransactionOptions NoPay -> ChainCursor -> { _originContract :: Address, _tokenId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
tokenPrice x0 cm r = uncurryFields  r $ tokenPrice' x0 cm
   where
    tokenPrice' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_originContract") Address) -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
    tokenPrice' y0 cm' y2 y3 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple2 y2 y3) :: TokenPriceFn)

--------------------------------------------------------------------------------
-- | TokenPriceFeeIncludedFn
--------------------------------------------------------------------------------


type TokenPriceFeeIncludedFn = Tagged (SProxy "tokenPriceFeeIncluded(address,uint256)") (Tuple2 (Tagged (SProxy "_originContract") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

tokenPriceFeeIncluded :: TransactionOptions NoPay -> ChainCursor -> { _originContract :: Address, _tokenId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
tokenPriceFeeIncluded x0 cm r = uncurryFields  r $ tokenPriceFeeIncluded' x0 cm
   where
    tokenPriceFeeIncluded' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_originContract") Address) -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
    tokenPriceFeeIncluded' y0 cm' y2 y3 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple2 y2 y3) :: TokenPriceFeeIncludedFn)

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

--------------------------------------------------------------------------------
-- | WithdrawPaymentsWithGasFn
--------------------------------------------------------------------------------


type WithdrawPaymentsWithGasFn = Tagged (SProxy "withdrawPaymentsWithGas(address)") (Tuple1 (Tagged (SProxy "payee") Address))

withdrawPaymentsWithGas :: TransactionOptions NoPay -> { payee :: Address } -> Web3 HexString
withdrawPaymentsWithGas x0 r = uncurryFields  r $ withdrawPaymentsWithGas' x0
   where
    withdrawPaymentsWithGas' :: TransactionOptions NoPay -> (Tagged (SProxy "payee") Address) -> Web3 HexString
    withdrawPaymentsWithGas' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: WithdrawPaymentsWithGasFn)