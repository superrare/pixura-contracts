--------------------------------------------------------------------------------
-- | Marketplace.MarketplaceSettings
--------------------------------------------------------------------------------
module Contracts.Marketplace.MarketplaceSettings where

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
import Network.Ethereum.Web3.Solidity (BytesN, D2, D3, D5, D6, D8, DOne, Tuple0(..), Tuple1(..), Tuple2(..), Tuple3(..), UIntN, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)

--------------------------------------------------------------------------------
-- | ConstructorFn
--------------------------------------------------------------------------------
type ConstructorFn
  = Tagged (SProxy "constructor()") (Tuple0)

constructor :: TransactionOptions NoPay -> HexString -> Web3 HexString
constructor x0 bc = deployContract x0 bc ((tagged $ Tuple0) :: ConstructorFn)

--------------------------------------------------------------------------------
-- | OwnershipTransferred
--------------------------------------------------------------------------------
newtype OwnershipTransferred
  = OwnershipTransferred { previousOwner :: Address, newOwner :: Address }

derive instance newtypeOwnershipTransferred :: Newtype OwnershipTransferred _

instance eventFilterOwnershipTransferred :: EventFilter OwnershipTransferred where
  eventFilter _ addr =
    defaultFilter
      # _address
      .~ Just addr
      # _topics
      .~ Just [ Just (unsafePartial $ fromJust $ mkHexString "8be0079c531659141344cd1fd0a4f28419497f9722a3daafe3b4186f6b6457e0"), Nothing, Nothing ]

instance indexedEventOwnershipTransferred :: IndexedEvent (Tuple2 (Tagged (SProxy "previousOwner") Address) (Tagged (SProxy "newOwner") Address)) (Tuple0) OwnershipTransferred where
  isAnonymous _ = false

derive instance genericOwnershipTransferred :: Generic OwnershipTransferred _

instance eventGenericOwnershipTransferredShow :: Show OwnershipTransferred where
  show = genericShow

instance eventGenericOwnershipTransferredeq :: Eq OwnershipTransferred where
  eq = genericEq

--------------------------------------------------------------------------------
-- | RoleAdminChanged
--------------------------------------------------------------------------------
newtype RoleAdminChanged
  = RoleAdminChanged { role :: (BytesN (D3 :& DOne D2)), previousAdminRole :: (BytesN (D3 :& DOne D2)), newAdminRole :: (BytesN (D3 :& DOne D2)) }

derive instance newtypeRoleAdminChanged :: Newtype RoleAdminChanged _

instance eventFilterRoleAdminChanged :: EventFilter RoleAdminChanged where
  eventFilter _ addr =
    defaultFilter
      # _address
      .~ Just addr
      # _topics
      .~ Just [ Just (unsafePartial $ fromJust $ mkHexString "bd79b86ffe0ab8e8776151514217cd7cacd52c909f66475c3af44e129f0b00ff"), Nothing, Nothing, Nothing ]

instance indexedEventRoleAdminChanged :: IndexedEvent (Tuple3 (Tagged (SProxy "role") (BytesN (D3 :& DOne D2))) (Tagged (SProxy "previousAdminRole") (BytesN (D3 :& DOne D2))) (Tagged (SProxy "newAdminRole") (BytesN (D3 :& DOne D2)))) (Tuple0) RoleAdminChanged where
  isAnonymous _ = false

derive instance genericRoleAdminChanged :: Generic RoleAdminChanged _

instance eventGenericRoleAdminChangedShow :: Show RoleAdminChanged where
  show = genericShow

instance eventGenericRoleAdminChangedeq :: Eq RoleAdminChanged where
  eq = genericEq

--------------------------------------------------------------------------------
-- | RoleGranted
--------------------------------------------------------------------------------
newtype RoleGranted
  = RoleGranted { role :: (BytesN (D3 :& DOne D2)), account :: Address, sender :: Address }

derive instance newtypeRoleGranted :: Newtype RoleGranted _

instance eventFilterRoleGranted :: EventFilter RoleGranted where
  eventFilter _ addr =
    defaultFilter
      # _address
      .~ Just addr
      # _topics
      .~ Just [ Just (unsafePartial $ fromJust $ mkHexString "2f8788117e7eff1d82e926ec794901d17c78024a50270940304540a733656f0d"), Nothing, Nothing, Nothing ]

instance indexedEventRoleGranted :: IndexedEvent (Tuple3 (Tagged (SProxy "role") (BytesN (D3 :& DOne D2))) (Tagged (SProxy "account") Address) (Tagged (SProxy "sender") Address)) (Tuple0) RoleGranted where
  isAnonymous _ = false

derive instance genericRoleGranted :: Generic RoleGranted _

instance eventGenericRoleGrantedShow :: Show RoleGranted where
  show = genericShow

instance eventGenericRoleGrantedeq :: Eq RoleGranted where
  eq = genericEq

--------------------------------------------------------------------------------
-- | RoleRevoked
--------------------------------------------------------------------------------
newtype RoleRevoked
  = RoleRevoked { role :: (BytesN (D3 :& DOne D2)), account :: Address, sender :: Address }

derive instance newtypeRoleRevoked :: Newtype RoleRevoked _

instance eventFilterRoleRevoked :: EventFilter RoleRevoked where
  eventFilter _ addr =
    defaultFilter
      # _address
      .~ Just addr
      # _topics
      .~ Just [ Just (unsafePartial $ fromJust $ mkHexString "f6391f5c32d9c69d2a47ea670b442974b53935d1edc7fd64eb21e047a839171b"), Nothing, Nothing, Nothing ]

instance indexedEventRoleRevoked :: IndexedEvent (Tuple3 (Tagged (SProxy "role") (BytesN (D3 :& DOne D2))) (Tagged (SProxy "account") Address) (Tagged (SProxy "sender") Address)) (Tuple0) RoleRevoked where
  isAnonymous _ = false

derive instance genericRoleRevoked :: Generic RoleRevoked _

instance eventGenericRoleRevokedShow :: Show RoleRevoked where
  show = genericShow

instance eventGenericRoleRevokedeq :: Eq RoleRevoked where
  eq = genericEq

--------------------------------------------------------------------------------
-- | DEFAULT_ADMIN_ROLEFn
--------------------------------------------------------------------------------
type DEFAULT_ADMIN_ROLEFn
  = Tagged (SProxy "DEFAULT_ADMIN_ROLE()") (Tuple0)

dEFAULT_ADMIN_ROLE :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (BytesN (D3 :& DOne D2)))
dEFAULT_ADMIN_ROLE x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0) :: DEFAULT_ADMIN_ROLEFn)

--------------------------------------------------------------------------------
-- | TOKEN_MARK_ROLEFn
--------------------------------------------------------------------------------
type TOKEN_MARK_ROLEFn
  = Tagged (SProxy "TOKEN_MARK_ROLE()") (Tuple0)

tOKEN_MARK_ROLE :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (BytesN (D3 :& DOne D2)))
tOKEN_MARK_ROLE x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0) :: TOKEN_MARK_ROLEFn)

--------------------------------------------------------------------------------
-- | CalculateMarketplaceFeeFn
--------------------------------------------------------------------------------
type CalculateMarketplaceFeeFn
  = Tagged (SProxy "calculateMarketplaceFee(uint256)") (Tuple1 (Tagged (SProxy "_amount") (UIntN (D2 :& D5 :& DOne D6))))

calculateMarketplaceFee :: TransactionOptions NoPay -> ChainCursor -> { _amount :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
calculateMarketplaceFee x0 cm r = uncurryFields r $ calculateMarketplaceFee' x0 cm
  where
  calculateMarketplaceFee' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_amount") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
  calculateMarketplaceFee' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 y2) :: CalculateMarketplaceFeeFn)

--------------------------------------------------------------------------------
-- | CalculatePrimarySaleFeeFn
--------------------------------------------------------------------------------
type CalculatePrimarySaleFeeFn
  = Tagged (SProxy "calculatePrimarySaleFee(address,uint256)") (Tuple2 (Tagged (SProxy "_contractAddress") Address) (Tagged (SProxy "_amount") (UIntN (D2 :& D5 :& DOne D6))))

calculatePrimarySaleFee :: TransactionOptions NoPay -> ChainCursor -> { _contractAddress :: Address, _amount :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
calculatePrimarySaleFee x0 cm r = uncurryFields r $ calculatePrimarySaleFee' x0 cm
  where
  calculatePrimarySaleFee' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_contractAddress") Address) -> (Tagged (SProxy "_amount") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
  calculatePrimarySaleFee' y0 cm' y2 y3 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple2 y2 y3) :: CalculatePrimarySaleFeeFn)

--------------------------------------------------------------------------------
-- | GetERC721ContractPrimarySaleFeePercentageFn
--------------------------------------------------------------------------------
type GetERC721ContractPrimarySaleFeePercentageFn
  = Tagged (SProxy "getERC721ContractPrimarySaleFeePercentage(address)") (Tuple1 (Tagged (SProxy "_contractAddress") Address))

getERC721ContractPrimarySaleFeePercentage :: TransactionOptions NoPay -> ChainCursor -> { _contractAddress :: Address } -> Web3 (Either CallError (UIntN (DOne D8)))
getERC721ContractPrimarySaleFeePercentage x0 cm r = uncurryFields r $ getERC721ContractPrimarySaleFeePercentage' x0 cm
  where
  getERC721ContractPrimarySaleFeePercentage' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_contractAddress") Address) -> Web3 (Either CallError (UIntN (DOne D8)))
  getERC721ContractPrimarySaleFeePercentage' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 y2) :: GetERC721ContractPrimarySaleFeePercentageFn)

--------------------------------------------------------------------------------
-- | GetMarketplaceFeePercentageFn
--------------------------------------------------------------------------------
type GetMarketplaceFeePercentageFn
  = Tagged (SProxy "getMarketplaceFeePercentage()") (Tuple0)

getMarketplaceFeePercentage :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN (DOne D8)))
getMarketplaceFeePercentage x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0) :: GetMarketplaceFeePercentageFn)

--------------------------------------------------------------------------------
-- | GetMarketplaceMaxValueFn
--------------------------------------------------------------------------------
type GetMarketplaceMaxValueFn
  = Tagged (SProxy "getMarketplaceMaxValue()") (Tuple0)

getMarketplaceMaxValue :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getMarketplaceMaxValue x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0) :: GetMarketplaceMaxValueFn)

--------------------------------------------------------------------------------
-- | GetMarketplaceMinValueFn
--------------------------------------------------------------------------------
type GetMarketplaceMinValueFn
  = Tagged (SProxy "getMarketplaceMinValue()") (Tuple0)

getMarketplaceMinValue :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getMarketplaceMinValue x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0) :: GetMarketplaceMinValueFn)

--------------------------------------------------------------------------------
-- | GetRoleAdminFn
--------------------------------------------------------------------------------
type GetRoleAdminFn
  = Tagged (SProxy "getRoleAdmin(bytes32)") (Tuple1 (Tagged (SProxy "role") (BytesN (D3 :& DOne D2))))

getRoleAdmin :: TransactionOptions NoPay -> ChainCursor -> { role :: (BytesN (D3 :& DOne D2)) } -> Web3 (Either CallError (BytesN (D3 :& DOne D2)))
getRoleAdmin x0 cm r = uncurryFields r $ getRoleAdmin' x0 cm
  where
  getRoleAdmin' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "role") (BytesN (D3 :& DOne D2))) -> Web3 (Either CallError (BytesN (D3 :& DOne D2)))
  getRoleAdmin' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 y2) :: GetRoleAdminFn)

--------------------------------------------------------------------------------
-- | GetRoleMemberFn
--------------------------------------------------------------------------------
type GetRoleMemberFn
  = Tagged (SProxy "getRoleMember(bytes32,uint256)") (Tuple2 (Tagged (SProxy "role") (BytesN (D3 :& DOne D2))) (Tagged (SProxy "index") (UIntN (D2 :& D5 :& DOne D6))))

getRoleMember :: TransactionOptions NoPay -> ChainCursor -> { role :: (BytesN (D3 :& DOne D2)), index :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 (Either CallError Address)
getRoleMember x0 cm r = uncurryFields r $ getRoleMember' x0 cm
  where
  getRoleMember' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "role") (BytesN (D3 :& DOne D2))) -> (Tagged (SProxy "index") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 (Either CallError Address)
  getRoleMember' y0 cm' y2 y3 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple2 y2 y3) :: GetRoleMemberFn)

--------------------------------------------------------------------------------
-- | GetRoleMemberCountFn
--------------------------------------------------------------------------------
type GetRoleMemberCountFn
  = Tagged (SProxy "getRoleMemberCount(bytes32)") (Tuple1 (Tagged (SProxy "role") (BytesN (D3 :& DOne D2))))

getRoleMemberCount :: TransactionOptions NoPay -> ChainCursor -> { role :: (BytesN (D3 :& DOne D2)) } -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getRoleMemberCount x0 cm r = uncurryFields r $ getRoleMemberCount' x0 cm
  where
  getRoleMemberCount' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "role") (BytesN (D3 :& DOne D2))) -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
  getRoleMemberCount' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 y2) :: GetRoleMemberCountFn)

--------------------------------------------------------------------------------
-- | GrantMarketplaceAccessFn
--------------------------------------------------------------------------------
type GrantMarketplaceAccessFn
  = Tagged (SProxy "grantMarketplaceAccess(address)") (Tuple1 (Tagged (SProxy "_account") Address))

grantMarketplaceAccess :: TransactionOptions NoPay -> { _account :: Address } -> Web3 HexString
grantMarketplaceAccess x0 r = uncurryFields r $ grantMarketplaceAccess' x0
  where
  grantMarketplaceAccess' :: TransactionOptions NoPay -> (Tagged (SProxy "_account") Address) -> Web3 HexString
  grantMarketplaceAccess' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: GrantMarketplaceAccessFn)

--------------------------------------------------------------------------------
-- | GrantRoleFn
--------------------------------------------------------------------------------
type GrantRoleFn
  = Tagged (SProxy "grantRole(bytes32,address)") (Tuple2 (Tagged (SProxy "role") (BytesN (D3 :& DOne D2))) (Tagged (SProxy "account") Address))

grantRole :: TransactionOptions NoPay -> { role :: (BytesN (D3 :& DOne D2)), account :: Address } -> Web3 HexString
grantRole x0 r = uncurryFields r $ grantRole' x0
  where
  grantRole' :: TransactionOptions NoPay -> (Tagged (SProxy "role") (BytesN (D3 :& DOne D2))) -> (Tagged (SProxy "account") Address) -> Web3 HexString
  grantRole' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 y1 y2) :: GrantRoleFn)

--------------------------------------------------------------------------------
-- | HasERC721TokenSoldFn
--------------------------------------------------------------------------------
type HasERC721TokenSoldFn
  = Tagged (SProxy "hasERC721TokenSold(address,uint256)") (Tuple2 (Tagged (SProxy "_contractAddress") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

hasERC721TokenSold :: TransactionOptions NoPay -> ChainCursor -> { _contractAddress :: Address, _tokenId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 (Either CallError Boolean)
hasERC721TokenSold x0 cm r = uncurryFields r $ hasERC721TokenSold' x0 cm
  where
  hasERC721TokenSold' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_contractAddress") Address) -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 (Either CallError Boolean)
  hasERC721TokenSold' y0 cm' y2 y3 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple2 y2 y3) :: HasERC721TokenSoldFn)

--------------------------------------------------------------------------------
-- | HasRoleFn
--------------------------------------------------------------------------------
type HasRoleFn
  = Tagged (SProxy "hasRole(bytes32,address)") (Tuple2 (Tagged (SProxy "role") (BytesN (D3 :& DOne D2))) (Tagged (SProxy "account") Address))

hasRole :: TransactionOptions NoPay -> ChainCursor -> { role :: (BytesN (D3 :& DOne D2)), account :: Address } -> Web3 (Either CallError Boolean)
hasRole x0 cm r = uncurryFields r $ hasRole' x0 cm
  where
  hasRole' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "role") (BytesN (D3 :& DOne D2))) -> (Tagged (SProxy "account") Address) -> Web3 (Either CallError Boolean)
  hasRole' y0 cm' y2 y3 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple2 y2 y3) :: HasRoleFn)

--------------------------------------------------------------------------------
-- | MarkERC721TokenFn
--------------------------------------------------------------------------------
type MarkERC721TokenFn
  = Tagged (SProxy "markERC721Token(address,uint256,bool)") (Tuple3 (Tagged (SProxy "_contractAddress") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "_hasSold") Boolean))

markERC721Token :: TransactionOptions NoPay -> { _contractAddress :: Address, _tokenId :: (UIntN (D2 :& D5 :& DOne D6)), _hasSold :: Boolean } -> Web3 HexString
markERC721Token x0 r = uncurryFields r $ markERC721Token' x0
  where
  markERC721Token' :: TransactionOptions NoPay -> (Tagged (SProxy "_contractAddress") Address) -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> (Tagged (SProxy "_hasSold") Boolean) -> Web3 HexString
  markERC721Token' y0 y1 y2 y3 = sendTx y0 ((tagged $ Tuple3 y1 y2 y3) :: MarkERC721TokenFn)

--------------------------------------------------------------------------------
-- | MarkTokensAsSoldFn
--------------------------------------------------------------------------------
type MarkTokensAsSoldFn
  = Tagged (SProxy "markTokensAsSold(address,uint256[])") (Tuple2 (Tagged (SProxy "_originContract") Address) (Tagged (SProxy "_tokenIds") (Array (UIntN (D2 :& D5 :& DOne D6)))))

markTokensAsSold :: TransactionOptions NoPay -> { _originContract :: Address, _tokenIds :: (Array (UIntN (D2 :& D5 :& DOne D6))) } -> Web3 HexString
markTokensAsSold x0 r = uncurryFields r $ markTokensAsSold' x0
  where
  markTokensAsSold' :: TransactionOptions NoPay -> (Tagged (SProxy "_originContract") Address) -> (Tagged (SProxy "_tokenIds") (Array (UIntN (D2 :& D5 :& DOne D6)))) -> Web3 HexString
  markTokensAsSold' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 y1 y2) :: MarkTokensAsSoldFn)

--------------------------------------------------------------------------------
-- | OwnerFn
--------------------------------------------------------------------------------
type OwnerFn
  = Tagged (SProxy "owner()") (Tuple0)

owner :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Address)
owner x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0) :: OwnerFn)

--------------------------------------------------------------------------------
-- | RenounceOwnershipFn
--------------------------------------------------------------------------------
type RenounceOwnershipFn
  = Tagged (SProxy "renounceOwnership()") (Tuple0)

renounceOwnership :: TransactionOptions NoPay -> Web3 HexString
renounceOwnership x0 = sendTx x0 ((tagged $ Tuple0) :: RenounceOwnershipFn)

--------------------------------------------------------------------------------
-- | RenounceRoleFn
--------------------------------------------------------------------------------
type RenounceRoleFn
  = Tagged (SProxy "renounceRole(bytes32,address)") (Tuple2 (Tagged (SProxy "role") (BytesN (D3 :& DOne D2))) (Tagged (SProxy "account") Address))

renounceRole :: TransactionOptions NoPay -> { role :: (BytesN (D3 :& DOne D2)), account :: Address } -> Web3 HexString
renounceRole x0 r = uncurryFields r $ renounceRole' x0
  where
  renounceRole' :: TransactionOptions NoPay -> (Tagged (SProxy "role") (BytesN (D3 :& DOne D2))) -> (Tagged (SProxy "account") Address) -> Web3 HexString
  renounceRole' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 y1 y2) :: RenounceRoleFn)

--------------------------------------------------------------------------------
-- | RevokeRoleFn
--------------------------------------------------------------------------------
type RevokeRoleFn
  = Tagged (SProxy "revokeRole(bytes32,address)") (Tuple2 (Tagged (SProxy "role") (BytesN (D3 :& DOne D2))) (Tagged (SProxy "account") Address))

revokeRole :: TransactionOptions NoPay -> { role :: (BytesN (D3 :& DOne D2)), account :: Address } -> Web3 HexString
revokeRole x0 r = uncurryFields r $ revokeRole' x0
  where
  revokeRole' :: TransactionOptions NoPay -> (Tagged (SProxy "role") (BytesN (D3 :& DOne D2))) -> (Tagged (SProxy "account") Address) -> Web3 HexString
  revokeRole' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 y1 y2) :: RevokeRoleFn)

--------------------------------------------------------------------------------
-- | SetERC721ContractPrimarySaleFeePercentageFn
--------------------------------------------------------------------------------
type SetERC721ContractPrimarySaleFeePercentageFn
  = Tagged (SProxy "setERC721ContractPrimarySaleFeePercentage(address,uint8)") (Tuple2 (Tagged (SProxy "_contractAddress") Address) (Tagged (SProxy "_percentage") (UIntN (DOne D8))))

setERC721ContractPrimarySaleFeePercentage :: TransactionOptions NoPay -> { _contractAddress :: Address, _percentage :: (UIntN (DOne D8)) } -> Web3 HexString
setERC721ContractPrimarySaleFeePercentage x0 r = uncurryFields r $ setERC721ContractPrimarySaleFeePercentage' x0
  where
  setERC721ContractPrimarySaleFeePercentage' :: TransactionOptions NoPay -> (Tagged (SProxy "_contractAddress") Address) -> (Tagged (SProxy "_percentage") (UIntN (DOne D8))) -> Web3 HexString
  setERC721ContractPrimarySaleFeePercentage' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 y1 y2) :: SetERC721ContractPrimarySaleFeePercentageFn)

--------------------------------------------------------------------------------
-- | SetMarketplaceFeePercentageFn
--------------------------------------------------------------------------------
type SetMarketplaceFeePercentageFn
  = Tagged (SProxy "setMarketplaceFeePercentage(uint8)") (Tuple1 (Tagged (SProxy "_percentage") (UIntN (DOne D8))))

setMarketplaceFeePercentage :: TransactionOptions NoPay -> { _percentage :: (UIntN (DOne D8)) } -> Web3 HexString
setMarketplaceFeePercentage x0 r = uncurryFields r $ setMarketplaceFeePercentage' x0
  where
  setMarketplaceFeePercentage' :: TransactionOptions NoPay -> (Tagged (SProxy "_percentage") (UIntN (DOne D8))) -> Web3 HexString
  setMarketplaceFeePercentage' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: SetMarketplaceFeePercentageFn)

--------------------------------------------------------------------------------
-- | SetMarketplaceMaxValueFn
--------------------------------------------------------------------------------
type SetMarketplaceMaxValueFn
  = Tagged (SProxy "setMarketplaceMaxValue(uint256)") (Tuple1 (Tagged (SProxy "_maxValue") (UIntN (D2 :& D5 :& DOne D6))))

setMarketplaceMaxValue :: TransactionOptions NoPay -> { _maxValue :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
setMarketplaceMaxValue x0 r = uncurryFields r $ setMarketplaceMaxValue' x0
  where
  setMarketplaceMaxValue' :: TransactionOptions NoPay -> (Tagged (SProxy "_maxValue") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
  setMarketplaceMaxValue' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: SetMarketplaceMaxValueFn)

--------------------------------------------------------------------------------
-- | SetMarketplaceMinValueFn
--------------------------------------------------------------------------------
type SetMarketplaceMinValueFn
  = Tagged (SProxy "setMarketplaceMinValue(uint256)") (Tuple1 (Tagged (SProxy "_minValue") (UIntN (D2 :& D5 :& DOne D6))))

setMarketplaceMinValue :: TransactionOptions NoPay -> { _minValue :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
setMarketplaceMinValue x0 r = uncurryFields r $ setMarketplaceMinValue' x0
  where
  setMarketplaceMinValue' :: TransactionOptions NoPay -> (Tagged (SProxy "_minValue") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
  setMarketplaceMinValue' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: SetMarketplaceMinValueFn)

--------------------------------------------------------------------------------
-- | TransferOwnershipFn
--------------------------------------------------------------------------------
type TransferOwnershipFn
  = Tagged (SProxy "transferOwnership(address)") (Tuple1 (Tagged (SProxy "newOwner") Address))

transferOwnership :: TransactionOptions NoPay -> { newOwner :: Address } -> Web3 HexString
transferOwnership x0 r = uncurryFields r $ transferOwnership' x0
  where
  transferOwnership' :: TransactionOptions NoPay -> (Tagged (SProxy "newOwner") Address) -> Web3 HexString
  transferOwnership' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: TransferOwnershipFn)
