--------------------------------------------------------------------------------
-- | SuperRareRoyaltyRegistry
--------------------------------------------------------------------------------

module Contracts.SuperRareRoyaltyRegistry where

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
import Network.Ethereum.Web3.Solidity (D2, D5, D6, D8, DOne, Tuple0(..), Tuple1(..), Tuple2(..), Tuple3(..), UIntN, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
--------------------------------------------------------------------------------
-- | ConstructorFn
--------------------------------------------------------------------------------


type ConstructorFn = Tagged (SProxy "constructor(address)") (Tuple1 (Tagged (SProxy "_iERC721TokenCreator") Address))

constructor :: TransactionOptions NoPay -> HexString -> { _iERC721TokenCreator :: Address } -> Web3 HexString
constructor x0 bc r = uncurryFields  r $ constructor' x0 bc
   where
    constructor' :: TransactionOptions NoPay -> HexString -> (Tagged (SProxy "_iERC721TokenCreator") Address) -> Web3 HexString
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
-- | CalculateRoyaltyFeeFn
--------------------------------------------------------------------------------


type CalculateRoyaltyFeeFn = Tagged (SProxy "calculateRoyaltyFee(address,uint256,uint256)") (Tuple3 (Tagged (SProxy "_contractAddress") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "_amount") (UIntN (D2 :& D5 :& DOne D6))))

calculateRoyaltyFee :: TransactionOptions NoPay -> ChainCursor -> { _contractAddress :: Address, _tokenId :: (UIntN (D2 :& D5 :& DOne D6)), _amount :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
calculateRoyaltyFee x0 cm r = uncurryFields  r $ calculateRoyaltyFee' x0 cm
   where
    calculateRoyaltyFee' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_contractAddress") Address) -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> (Tagged (SProxy "_amount") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
    calculateRoyaltyFee' y0 cm' y2 y3 y4 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple3 y2 y3 y4) :: CalculateRoyaltyFeeFn)

--------------------------------------------------------------------------------
-- | GetERC721TokenRoyaltyPercentageFn
--------------------------------------------------------------------------------


type GetERC721TokenRoyaltyPercentageFn = Tagged (SProxy "getERC721TokenRoyaltyPercentage(address,uint256)") (Tuple2 (Tagged (SProxy "_contractAddress") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

getERC721TokenRoyaltyPercentage :: TransactionOptions NoPay -> ChainCursor -> { _contractAddress :: Address, _tokenId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 (Either CallError (UIntN (DOne D8)))
getERC721TokenRoyaltyPercentage x0 cm r = uncurryFields  r $ getERC721TokenRoyaltyPercentage' x0 cm
   where
    getERC721TokenRoyaltyPercentage' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_contractAddress") Address) -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 (Either CallError (UIntN (DOne D8)))
    getERC721TokenRoyaltyPercentage' y0 cm' y2 y3 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple2 y2 y3) :: GetERC721TokenRoyaltyPercentageFn)

--------------------------------------------------------------------------------
-- | GetPercentageForSetERC721ContractRoyaltyFn
--------------------------------------------------------------------------------


type GetPercentageForSetERC721ContractRoyaltyFn = Tagged (SProxy "getPercentageForSetERC721ContractRoyalty(address)") (Tuple1 (Tagged (SProxy "_contractAddress") Address))

getPercentageForSetERC721ContractRoyalty :: TransactionOptions NoPay -> ChainCursor -> { _contractAddress :: Address } -> Web3 (Either CallError (UIntN (DOne D8)))
getPercentageForSetERC721ContractRoyalty x0 cm r = uncurryFields  r $ getPercentageForSetERC721ContractRoyalty' x0 cm
   where
    getPercentageForSetERC721ContractRoyalty' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_contractAddress") Address) -> Web3 (Either CallError (UIntN (DOne D8)))
    getPercentageForSetERC721ContractRoyalty' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 y2) :: GetPercentageForSetERC721ContractRoyaltyFn)

--------------------------------------------------------------------------------
-- | GetPercentageForSetERC721CreatorRoyaltyFn
--------------------------------------------------------------------------------


type GetPercentageForSetERC721CreatorRoyaltyFn = Tagged (SProxy "getPercentageForSetERC721CreatorRoyalty(address,uint256)") (Tuple2 (Tagged (SProxy "_contractAddress") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

getPercentageForSetERC721CreatorRoyalty :: TransactionOptions NoPay -> ChainCursor -> { _contractAddress :: Address, _tokenId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 (Either CallError (UIntN (DOne D8)))
getPercentageForSetERC721CreatorRoyalty x0 cm r = uncurryFields  r $ getPercentageForSetERC721CreatorRoyalty' x0 cm
   where
    getPercentageForSetERC721CreatorRoyalty' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_contractAddress") Address) -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 (Either CallError (UIntN (DOne D8)))
    getPercentageForSetERC721CreatorRoyalty' y0 cm' y2 y3 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple2 y2 y3) :: GetPercentageForSetERC721CreatorRoyaltyFn)

--------------------------------------------------------------------------------
-- | GetPercentageForSetERC721TokenRoyaltyFn
--------------------------------------------------------------------------------


type GetPercentageForSetERC721TokenRoyaltyFn = Tagged (SProxy "getPercentageForSetERC721TokenRoyalty(address,uint256)") (Tuple2 (Tagged (SProxy "_contractAddress") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

getPercentageForSetERC721TokenRoyalty :: TransactionOptions NoPay -> ChainCursor -> { _contractAddress :: Address, _tokenId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 (Either CallError (UIntN (DOne D8)))
getPercentageForSetERC721TokenRoyalty x0 cm r = uncurryFields  r $ getPercentageForSetERC721TokenRoyalty' x0 cm
   where
    getPercentageForSetERC721TokenRoyalty' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_contractAddress") Address) -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 (Either CallError (UIntN (DOne D8)))
    getPercentageForSetERC721TokenRoyalty' y0 cm' y2 y3 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple2 y2 y3) :: GetPercentageForSetERC721TokenRoyaltyFn)

--------------------------------------------------------------------------------
-- | IERC721TokenCreatorFn
--------------------------------------------------------------------------------


type IERC721TokenCreatorFn = Tagged (SProxy "iERC721TokenCreator()") (Tuple0 )

iERC721TokenCreator :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Address)
iERC721TokenCreator x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: IERC721TokenCreatorFn)

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
-- | SetIERC721TokenCreatorFn
--------------------------------------------------------------------------------


type SetIERC721TokenCreatorFn = Tagged (SProxy "setIERC721TokenCreator(address)") (Tuple1 (Tagged (SProxy "_contractAddress") Address))

setIERC721TokenCreator :: TransactionOptions NoPay -> { _contractAddress :: Address } -> Web3 HexString
setIERC721TokenCreator x0 r = uncurryFields  r $ setIERC721TokenCreator' x0
   where
    setIERC721TokenCreator' :: TransactionOptions NoPay -> (Tagged (SProxy "_contractAddress") Address) -> Web3 HexString
    setIERC721TokenCreator' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: SetIERC721TokenCreatorFn)

--------------------------------------------------------------------------------
-- | SetPercentageForSetERC721ContractRoyaltyFn
--------------------------------------------------------------------------------


type SetPercentageForSetERC721ContractRoyaltyFn = Tagged (SProxy "setPercentageForSetERC721ContractRoyalty(address,uint8)") (Tuple2 (Tagged (SProxy "_contractAddress") Address) (Tagged (SProxy "_percentage") (UIntN (DOne D8))))

setPercentageForSetERC721ContractRoyalty :: TransactionOptions NoPay -> { _contractAddress :: Address, _percentage :: (UIntN (DOne D8)) } -> Web3 HexString
setPercentageForSetERC721ContractRoyalty x0 r = uncurryFields  r $ setPercentageForSetERC721ContractRoyalty' x0
   where
    setPercentageForSetERC721ContractRoyalty' :: TransactionOptions NoPay -> (Tagged (SProxy "_contractAddress") Address) -> (Tagged (SProxy "_percentage") (UIntN (DOne D8))) -> Web3 HexString
    setPercentageForSetERC721ContractRoyalty' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 y1 y2) :: SetPercentageForSetERC721ContractRoyaltyFn)

--------------------------------------------------------------------------------
-- | SetPercentageForSetERC721CreatorRoyaltyFn
--------------------------------------------------------------------------------


type SetPercentageForSetERC721CreatorRoyaltyFn = Tagged (SProxy "setPercentageForSetERC721CreatorRoyalty(address,uint8)") (Tuple2 (Tagged (SProxy "_creatorAddress") Address) (Tagged (SProxy "_percentage") (UIntN (DOne D8))))

setPercentageForSetERC721CreatorRoyalty :: TransactionOptions NoPay -> { _creatorAddress :: Address, _percentage :: (UIntN (DOne D8)) } -> Web3 HexString
setPercentageForSetERC721CreatorRoyalty x0 r = uncurryFields  r $ setPercentageForSetERC721CreatorRoyalty' x0
   where
    setPercentageForSetERC721CreatorRoyalty' :: TransactionOptions NoPay -> (Tagged (SProxy "_creatorAddress") Address) -> (Tagged (SProxy "_percentage") (UIntN (DOne D8))) -> Web3 HexString
    setPercentageForSetERC721CreatorRoyalty' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 y1 y2) :: SetPercentageForSetERC721CreatorRoyaltyFn)

--------------------------------------------------------------------------------
-- | SetPercentageForSetERC721TokenRoyaltyFn
--------------------------------------------------------------------------------


type SetPercentageForSetERC721TokenRoyaltyFn = Tagged (SProxy "setPercentageForSetERC721TokenRoyalty(address,uint256,uint8)") (Tuple3 (Tagged (SProxy "_contractAddress") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "_percentage") (UIntN (DOne D8))))

setPercentageForSetERC721TokenRoyalty :: TransactionOptions NoPay -> { _contractAddress :: Address, _tokenId :: (UIntN (D2 :& D5 :& DOne D6)), _percentage :: (UIntN (DOne D8)) } -> Web3 HexString
setPercentageForSetERC721TokenRoyalty x0 r = uncurryFields  r $ setPercentageForSetERC721TokenRoyalty' x0
   where
    setPercentageForSetERC721TokenRoyalty' :: TransactionOptions NoPay -> (Tagged (SProxy "_contractAddress") Address) -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> (Tagged (SProxy "_percentage") (UIntN (DOne D8))) -> Web3 HexString
    setPercentageForSetERC721TokenRoyalty' y0 y1 y2 y3 = sendTx y0 ((tagged $ Tuple3 y1 y2 y3) :: SetPercentageForSetERC721TokenRoyaltyFn)

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