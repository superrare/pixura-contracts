--------------------------------------------------------------------------------
-- | SuperRareTokenCreatorRegistry
--------------------------------------------------------------------------------

module Contracts.SuperRareTokenCreatorRegistry where

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
import Network.Ethereum.Web3.Solidity (D2, D5, D6, DOne, Tuple0(..), Tuple1(..), Tuple2(..), Tuple3(..), UIntN, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
--------------------------------------------------------------------------------
-- | ConstructorFn
--------------------------------------------------------------------------------


type ConstructorFn = Tagged (SProxy "constructor(address[])") (Tuple1 (Tagged (SProxy "_iERC721Creators") (Array Address)))

constructor :: TransactionOptions NoPay -> HexString -> { _iERC721Creators :: (Array Address) } -> Web3 HexString
constructor x0 bc r = uncurryFields  r $ constructor' x0 bc
   where
    constructor' :: TransactionOptions NoPay -> HexString -> (Tagged (SProxy "_iERC721Creators") (Array Address)) -> Web3 HexString
    constructor' y0 bc' y2 = deployContract y0 bc' ((tagged $ Tuple1 y2) :: ConstructorFn)

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
-- | SetIERC721CreatorFn
--------------------------------------------------------------------------------


type SetIERC721CreatorFn = Tagged (SProxy "setIERC721Creator(address)") (Tuple1 (Tagged (SProxy "_contractAddress") Address))

setIERC721Creator :: TransactionOptions NoPay -> { _contractAddress :: Address } -> Web3 HexString
setIERC721Creator x0 r = uncurryFields  r $ setIERC721Creator' x0
   where
    setIERC721Creator' :: TransactionOptions NoPay -> (Tagged (SProxy "_contractAddress") Address) -> Web3 HexString
    setIERC721Creator' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: SetIERC721CreatorFn)

--------------------------------------------------------------------------------
-- | SetTokenCreatorFn
--------------------------------------------------------------------------------


type SetTokenCreatorFn = Tagged (SProxy "setTokenCreator(address,uint256,address)") (Tuple3 (Tagged (SProxy "_contractAddress") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "_creator") Address))

setTokenCreator :: TransactionOptions NoPay -> { _contractAddress :: Address, _tokenId :: (UIntN (D2 :& D5 :& DOne D6)), _creator :: Address } -> Web3 HexString
setTokenCreator x0 r = uncurryFields  r $ setTokenCreator' x0
   where
    setTokenCreator' :: TransactionOptions NoPay -> (Tagged (SProxy "_contractAddress") Address) -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> (Tagged (SProxy "_creator") Address) -> Web3 HexString
    setTokenCreator' y0 y1 y2 y3 = sendTx y0 ((tagged $ Tuple3 y1 y2 y3) :: SetTokenCreatorFn)

--------------------------------------------------------------------------------
-- | TokenCreatorFn
--------------------------------------------------------------------------------


type TokenCreatorFn = Tagged (SProxy "tokenCreator(address,uint256)") (Tuple2 (Tagged (SProxy "_contractAddress") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

tokenCreator :: TransactionOptions NoPay -> ChainCursor -> { _contractAddress :: Address, _tokenId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 (Either CallError Address)
tokenCreator x0 cm r = uncurryFields  r $ tokenCreator' x0 cm
   where
    tokenCreator' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_contractAddress") Address) -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 (Either CallError Address)
    tokenCreator' y0 cm' y2 y3 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple2 y2 y3) :: TokenCreatorFn)

--------------------------------------------------------------------------------
-- | TransferOwnershipFn
--------------------------------------------------------------------------------


type TransferOwnershipFn = Tagged (SProxy "transferOwnership(address)") (Tuple1 (Tagged (SProxy "newOwner") Address))

transferOwnership :: TransactionOptions NoPay -> { newOwner :: Address } -> Web3 HexString
transferOwnership x0 r = uncurryFields  r $ transferOwnership' x0
   where
    transferOwnership' :: TransactionOptions NoPay -> (Tagged (SProxy "newOwner") Address) -> Web3 HexString
    transferOwnership' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: TransferOwnershipFn)