--------------------------------------------------------------------------------
-- | IERC721CreatorRoyalty
--------------------------------------------------------------------------------

module Contracts.IERC721CreatorRoyalty where

import Prelude 

import Data.Either (Either)
import Data.Functor.Tagged (Tagged, tagged)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (call)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (D2, D5, D6, D8, DOne, Tuple2(..), Tuple3(..), UIntN, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, NoPay, TransactionOptions, Web3)
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
-- | TokenCreatorFn
--------------------------------------------------------------------------------


type TokenCreatorFn = Tagged (SProxy "tokenCreator(address,uint256)") (Tuple2 (Tagged (SProxy "_contractAddress") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

tokenCreator :: TransactionOptions NoPay -> ChainCursor -> { _contractAddress :: Address, _tokenId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 (Either CallError Address)
tokenCreator x0 cm r = uncurryFields  r $ tokenCreator' x0 cm
   where
    tokenCreator' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_contractAddress") Address) -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 (Either CallError Address)
    tokenCreator' y0 cm' y2 y3 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple2 y2 y3) :: TokenCreatorFn)