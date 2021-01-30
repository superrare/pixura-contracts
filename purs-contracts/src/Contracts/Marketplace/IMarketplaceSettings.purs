--------------------------------------------------------------------------------
-- | Marketplace.IMarketplaceSettings
--------------------------------------------------------------------------------

module Contracts.Marketplace.IMarketplaceSettings where

import Prelude 

import Data.Either (Either)
import Data.Functor.Tagged (Tagged, tagged)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (call, sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (D2, D5, D6, D8, DOne, Tuple0(..), Tuple1(..), Tuple2(..), Tuple3(..), UIntN, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3)
--------------------------------------------------------------------------------
-- | CalculateMarketplaceFeeFn
--------------------------------------------------------------------------------


type CalculateMarketplaceFeeFn = Tagged (SProxy "calculateMarketplaceFee(uint256)") (Tuple1 (Tagged (SProxy "_amount") (UIntN (D2 :& D5 :& DOne D6))))

calculateMarketplaceFee :: TransactionOptions NoPay -> ChainCursor -> { _amount :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
calculateMarketplaceFee x0 cm r = uncurryFields  r $ calculateMarketplaceFee' x0 cm
   where
    calculateMarketplaceFee' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_amount") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
    calculateMarketplaceFee' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 y2) :: CalculateMarketplaceFeeFn)

--------------------------------------------------------------------------------
-- | CalculatePrimarySaleFeeFn
--------------------------------------------------------------------------------


type CalculatePrimarySaleFeeFn = Tagged (SProxy "calculatePrimarySaleFee(address,uint256)") (Tuple2 (Tagged (SProxy "_contractAddress") Address) (Tagged (SProxy "_amount") (UIntN (D2 :& D5 :& DOne D6))))

calculatePrimarySaleFee :: TransactionOptions NoPay -> ChainCursor -> { _contractAddress :: Address, _amount :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
calculatePrimarySaleFee x0 cm r = uncurryFields  r $ calculatePrimarySaleFee' x0 cm
   where
    calculatePrimarySaleFee' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_contractAddress") Address) -> (Tagged (SProxy "_amount") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
    calculatePrimarySaleFee' y0 cm' y2 y3 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple2 y2 y3) :: CalculatePrimarySaleFeeFn)

--------------------------------------------------------------------------------
-- | GetERC721ContractPrimarySaleFeePercentageFn
--------------------------------------------------------------------------------


type GetERC721ContractPrimarySaleFeePercentageFn = Tagged (SProxy "getERC721ContractPrimarySaleFeePercentage(address)") (Tuple1 (Tagged (SProxy "_contractAddress") Address))

getERC721ContractPrimarySaleFeePercentage :: TransactionOptions NoPay -> ChainCursor -> { _contractAddress :: Address } -> Web3 (Either CallError (UIntN (DOne D8)))
getERC721ContractPrimarySaleFeePercentage x0 cm r = uncurryFields  r $ getERC721ContractPrimarySaleFeePercentage' x0 cm
   where
    getERC721ContractPrimarySaleFeePercentage' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_contractAddress") Address) -> Web3 (Either CallError (UIntN (DOne D8)))
    getERC721ContractPrimarySaleFeePercentage' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 y2) :: GetERC721ContractPrimarySaleFeePercentageFn)

--------------------------------------------------------------------------------
-- | GetMarketplaceFeePercentageFn
--------------------------------------------------------------------------------


type GetMarketplaceFeePercentageFn = Tagged (SProxy "getMarketplaceFeePercentage()") (Tuple0 )

getMarketplaceFeePercentage :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN (DOne D8)))
getMarketplaceFeePercentage x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetMarketplaceFeePercentageFn)

--------------------------------------------------------------------------------
-- | GetMarketplaceMaxValueFn
--------------------------------------------------------------------------------


type GetMarketplaceMaxValueFn = Tagged (SProxy "getMarketplaceMaxValue()") (Tuple0 )

getMarketplaceMaxValue :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getMarketplaceMaxValue x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetMarketplaceMaxValueFn)

--------------------------------------------------------------------------------
-- | GetMarketplaceMinValueFn
--------------------------------------------------------------------------------


type GetMarketplaceMinValueFn = Tagged (SProxy "getMarketplaceMinValue()") (Tuple0 )

getMarketplaceMinValue :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
getMarketplaceMinValue x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetMarketplaceMinValueFn)

--------------------------------------------------------------------------------
-- | HasERC721TokenSoldFn
--------------------------------------------------------------------------------


type HasERC721TokenSoldFn = Tagged (SProxy "hasERC721TokenSold(address,uint256)") (Tuple2 (Tagged (SProxy "_contractAddress") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

hasERC721TokenSold :: TransactionOptions NoPay -> ChainCursor -> { _contractAddress :: Address, _tokenId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 (Either CallError Boolean)
hasERC721TokenSold x0 cm r = uncurryFields  r $ hasERC721TokenSold' x0 cm
   where
    hasERC721TokenSold' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_contractAddress") Address) -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 (Either CallError Boolean)
    hasERC721TokenSold' y0 cm' y2 y3 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple2 y2 y3) :: HasERC721TokenSoldFn)

--------------------------------------------------------------------------------
-- | MarkERC721TokenFn
--------------------------------------------------------------------------------


type MarkERC721TokenFn = Tagged (SProxy "markERC721Token(address,uint256,bool)") (Tuple3 (Tagged (SProxy "_contractAddress") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "_hasSold") Boolean))

markERC721Token :: TransactionOptions NoPay -> { _contractAddress :: Address, _tokenId :: (UIntN (D2 :& D5 :& DOne D6)), _hasSold :: Boolean } -> Web3 HexString
markERC721Token x0 r = uncurryFields  r $ markERC721Token' x0
   where
    markERC721Token' :: TransactionOptions NoPay -> (Tagged (SProxy "_contractAddress") Address) -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> (Tagged (SProxy "_hasSold") Boolean) -> Web3 HexString
    markERC721Token' y0 y1 y2 y3 = sendTx y0 ((tagged $ Tuple3 y1 y2 y3) :: MarkERC721TokenFn)