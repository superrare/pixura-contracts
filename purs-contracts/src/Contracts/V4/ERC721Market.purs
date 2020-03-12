--------------------------------------------------------------------------------
-- | ERC721Market
--------------------------------------------------------------------------------

module Contracts.V4.ERC721Market where

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
-- | BuyFn
--------------------------------------------------------------------------------


type BuyFn = Tagged (SProxy "buy(address,uint256)") (Tuple2 (Tagged (SProxy "_originContract") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

buy :: TransactionOptions MinorUnit -> { _originContract :: Address, _tokenId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
buy x0 r = uncurryFields  r $ buy' x0
   where
    buy' :: TransactionOptions MinorUnit -> (Tagged (SProxy "_originContract") Address) -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    buy' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 y1 y2) :: BuyFn)

--------------------------------------------------------------------------------
-- | IsOwnerFn
--------------------------------------------------------------------------------


type IsOwnerFn = Tagged (SProxy "isOwner()") (Tuple0 )

isOwner :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Boolean)
isOwner x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: IsOwnerFn)

--------------------------------------------------------------------------------
-- | OwnerFn
--------------------------------------------------------------------------------


type OwnerFn = Tagged (SProxy "owner()") (Tuple0 )

owner :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Address)
owner x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: OwnerFn)

--------------------------------------------------------------------------------
-- | RenounceOwnershipFn
--------------------------------------------------------------------------------


type RenounceOwnershipFn = Tagged (SProxy "renounceOwnership()") (Tuple0 )

renounceOwnership :: TransactionOptions NoPay -> Web3 HexString
renounceOwnership x0 = sendTx x0 ((tagged $ Tuple0 ) :: RenounceOwnershipFn)

--------------------------------------------------------------------------------
-- | SetSalePriceFn
--------------------------------------------------------------------------------


type SetSalePriceFn = Tagged (SProxy "setSalePrice(address,uint256,uint256)") (Tuple3 (Tagged (SProxy "_originContract") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "_amount") (UIntN (D2 :& D5 :& DOne D6))))

setSalePrice :: TransactionOptions MinorUnit -> { _originContract :: Address, _tokenId :: (UIntN (D2 :& D5 :& DOne D6)), _amount :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
setSalePrice x0 r = uncurryFields  r $ setSalePrice' x0
   where
    setSalePrice' :: TransactionOptions MinorUnit -> (Tagged (SProxy "_originContract") Address) -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> (Tagged (SProxy "_amount") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
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
-- | TransferOwnershipFn
--------------------------------------------------------------------------------


type TransferOwnershipFn = Tagged (SProxy "transferOwnership(address)") (Tuple1 (Tagged (SProxy "newOwner") Address))

transferOwnership :: TransactionOptions NoPay -> { newOwner :: Address } -> Web3 HexString
transferOwnership x0 r = uncurryFields  r $ transferOwnership' x0
   where
    transferOwnership' :: TransactionOptions NoPay -> (Tagged (SProxy "newOwner") Address) -> Web3 HexString
    transferOwnership' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: TransferOwnershipFn)