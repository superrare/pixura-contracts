--------------------------------------------------------------------------------
-- | PixuraNFTContractGenerator
--------------------------------------------------------------------------------

module Contracts.PixuraNFTContractGenerator where

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
import Network.Ethereum.Web3.Solidity (D2, D5, D6, DOne, Tuple0(..), Tuple1(..), Tuple2(..), UIntN, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Network.Ethereum.Web3.Types.TokenUnit (MinorUnit)
import Partial.Unsafe (unsafePartial)
--------------------------------------------------------------------------------
-- | ConstructorFn
--------------------------------------------------------------------------------


type ConstructorFn = Tagged (SProxy "constructor(uint256,uint256)") (Tuple2 (Tagged (SProxy "_operationCost") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "_nftOperationCost") (UIntN (D2 :& D5 :& DOne D6))))

constructor :: TransactionOptions NoPay -> HexString -> { _operationCost :: (UIntN (D2 :& D5 :& DOne D6)), _nftOperationCost :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
constructor x0 bc r = uncurryFields  r $ constructor' x0 bc
   where
    constructor' :: TransactionOptions NoPay -> HexString -> (Tagged (SProxy "_operationCost") (UIntN (D2 :& D5 :& DOne D6))) -> (Tagged (SProxy "_nftOperationCost") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    constructor' y0 bc' y2 y3 = deployContract y0 bc' ((tagged $ Tuple2 y2 y3) :: ConstructorFn)

--------------------------------------------------------------------------------
-- | PixuraNFTContractCreated
--------------------------------------------------------------------------------


newtype PixuraNFTContractCreated = PixuraNFTContractCreated {_contractAddress :: Address,_owner :: Address}

derive instance newtypePixuraNFTContractCreated :: Newtype PixuraNFTContractCreated _

instance eventFilterPixuraNFTContractCreated :: EventFilter PixuraNFTContractCreated where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "881314b912dd33ad3324ab01ca953fddb5d8c197f5b3dace581dc8af49cd8e12"),Nothing,Nothing]

instance indexedEventPixuraNFTContractCreated :: IndexedEvent (Tuple2 (Tagged (SProxy "_contractAddress") Address) (Tagged (SProxy "_owner") Address)) (Tuple0 ) PixuraNFTContractCreated where
  isAnonymous _ = false

derive instance genericPixuraNFTContractCreated :: Generic PixuraNFTContractCreated _

instance eventGenericPixuraNFTContractCreatedShow :: Show PixuraNFTContractCreated where
  show = genericShow

instance eventGenericPixuraNFTContractCreatedeq :: Eq PixuraNFTContractCreated where
  eq = genericEq

--------------------------------------------------------------------------------
-- | OperatorSet
--------------------------------------------------------------------------------


newtype OperatorSet = OperatorSet {_operator :: Address}

derive instance newtypeOperatorSet :: Newtype OperatorSet _

instance eventFilterOperatorSet :: EventFilter OperatorSet where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "99d737e0adf2c449d71890b86772885ec7959b152ddb265f76325b6e68e105d3"),Nothing]

instance indexedEventOperatorSet :: IndexedEvent (Tuple1 (Tagged (SProxy "_operator") Address)) (Tuple0 ) OperatorSet where
  isAnonymous _ = false

derive instance genericOperatorSet :: Generic OperatorSet _

instance eventGenericOperatorSetShow :: Show OperatorSet where
  show = genericShow

instance eventGenericOperatorSeteq :: Eq OperatorSet where
  eq = genericEq

--------------------------------------------------------------------------------
-- | OperationCostSet
--------------------------------------------------------------------------------


newtype OperationCostSet = OperationCostSet {_operationCost :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeOperationCostSet :: Newtype OperationCostSet _

instance eventFilterOperationCostSet :: EventFilter OperationCostSet where
  eventFilter _ addr = defaultFilter
    # _address .~ Just addr
    # _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "70524409f1852b9903371526eb3765eb5462f72a3df57469113284ab8a76ec95"),Nothing]

instance indexedEventOperationCostSet :: IndexedEvent (Tuple1 (Tagged (SProxy "_operationCost") (UIntN (D2 :& D5 :& DOne D6)))) (Tuple0 ) OperationCostSet where
  isAnonymous _ = false

derive instance genericOperationCostSet :: Generic OperationCostSet _

instance eventGenericOperationCostSetShow :: Show OperationCostSet where
  show = genericShow

instance eventGenericOperationCostSeteq :: Eq OperationCostSet where
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
-- | CreateNFTContractFn
--------------------------------------------------------------------------------


type CreateNFTContractFn = Tagged (SProxy "createNFTContract(string,string)") (Tuple2 (Tagged (SProxy "_name") String) (Tagged (SProxy "_symbol") String))

createNFTContract :: TransactionOptions MinorUnit -> { _name :: String, _symbol :: String } -> Web3 HexString
createNFTContract x0 r = uncurryFields  r $ createNFTContract' x0
   where
    createNFTContract' :: TransactionOptions MinorUnit -> (Tagged (SProxy "_name") String) -> (Tagged (SProxy "_symbol") String) -> Web3 HexString
    createNFTContract' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 y1 y2) :: CreateNFTContractFn)

--------------------------------------------------------------------------------
-- | IsOwnerFn
--------------------------------------------------------------------------------


type IsOwnerFn = Tagged (SProxy "isOwner()") (Tuple0 )

isOwner :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Boolean)
isOwner x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: IsOwnerFn)

--------------------------------------------------------------------------------
-- | NftOperationCostFn
--------------------------------------------------------------------------------


type NftOperationCostFn = Tagged (SProxy "nftOperationCost()") (Tuple0 )

nftOperationCost :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
nftOperationCost x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: NftOperationCostFn)

--------------------------------------------------------------------------------
-- | OperationCostFn
--------------------------------------------------------------------------------


type OperationCostFn = Tagged (SProxy "operationCost()") (Tuple0 )

operationCost :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
operationCost x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: OperationCostFn)

--------------------------------------------------------------------------------
-- | OperatorFn
--------------------------------------------------------------------------------


type OperatorFn = Tagged (SProxy "operator()") (Tuple0 )

operator :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Address)
operator x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: OperatorFn)

--------------------------------------------------------------------------------
-- | OwnerFn
--------------------------------------------------------------------------------


type OwnerFn = Tagged (SProxy "owner()") (Tuple0 )

owner :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Address)
owner x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: OwnerFn)

--------------------------------------------------------------------------------
-- | RemoveOperatorFn
--------------------------------------------------------------------------------


type RemoveOperatorFn = Tagged (SProxy "removeOperator()") (Tuple0 )

removeOperator :: TransactionOptions NoPay -> Web3 HexString
removeOperator x0 = sendTx x0 ((tagged $ Tuple0 ) :: RemoveOperatorFn)

--------------------------------------------------------------------------------
-- | RenounceOwnershipFn
--------------------------------------------------------------------------------


type RenounceOwnershipFn = Tagged (SProxy "renounceOwnership()") (Tuple0 )

renounceOwnership :: TransactionOptions NoPay -> Web3 HexString
renounceOwnership x0 = sendTx x0 ((tagged $ Tuple0 ) :: RenounceOwnershipFn)

--------------------------------------------------------------------------------
-- | SetNftOperationCostFn
--------------------------------------------------------------------------------


type SetNftOperationCostFn = Tagged (SProxy "setNftOperationCost(uint256)") (Tuple1 (Tagged (SProxy "_cost") (UIntN (D2 :& D5 :& DOne D6))))

setNftOperationCost :: TransactionOptions NoPay -> { _cost :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
setNftOperationCost x0 r = uncurryFields  r $ setNftOperationCost' x0
   where
    setNftOperationCost' :: TransactionOptions NoPay -> (Tagged (SProxy "_cost") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    setNftOperationCost' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: SetNftOperationCostFn)

--------------------------------------------------------------------------------
-- | SetOperationCostFn
--------------------------------------------------------------------------------


type SetOperationCostFn = Tagged (SProxy "setOperationCost(uint256)") (Tuple1 (Tagged (SProxy "_operationCost") (UIntN (D2 :& D5 :& DOne D6))))

setOperationCost :: TransactionOptions NoPay -> { _operationCost :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
setOperationCost x0 r = uncurryFields  r $ setOperationCost' x0
   where
    setOperationCost' :: TransactionOptions NoPay -> (Tagged (SProxy "_operationCost") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    setOperationCost' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: SetOperationCostFn)

--------------------------------------------------------------------------------
-- | SetOperatorFn
--------------------------------------------------------------------------------


type SetOperatorFn = Tagged (SProxy "setOperator(address)") (Tuple1 (Tagged (SProxy "_operator") Address))

setOperator :: TransactionOptions NoPay -> { _operator :: Address } -> Web3 HexString
setOperator x0 r = uncurryFields  r $ setOperator' x0
   where
    setOperator' :: TransactionOptions NoPay -> (Tagged (SProxy "_operator") Address) -> Web3 HexString
    setOperator' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: SetOperatorFn)

--------------------------------------------------------------------------------
-- | TransferOwnershipFn
--------------------------------------------------------------------------------


type TransferOwnershipFn = Tagged (SProxy "transferOwnership(address)") (Tuple1 (Tagged (SProxy "newOwner") Address))

transferOwnership :: TransactionOptions NoPay -> { newOwner :: Address } -> Web3 HexString
transferOwnership x0 r = uncurryFields  r $ transferOwnership' x0
   where
    transferOwnership' :: TransactionOptions NoPay -> (Tagged (SProxy "newOwner") Address) -> Web3 HexString
    transferOwnership' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: TransferOwnershipFn)