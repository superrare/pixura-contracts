--------------------------------------------------------------------------------
-- | IERC721TokenCreator
--------------------------------------------------------------------------------

module Contracts.IERC721TokenCreator where

import Prelude 

import Data.Either (Either)
import Data.Functor.Tagged (Tagged, tagged)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (call)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (D2, D5, D6, DOne, Tuple2(..), UIntN, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, NoPay, TransactionOptions, Web3)
--------------------------------------------------------------------------------
-- | TokenCreatorFn
--------------------------------------------------------------------------------


type TokenCreatorFn = Tagged (SProxy "tokenCreator(address,uint256)") (Tuple2 (Tagged (SProxy "_contractAddress") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))))

tokenCreator :: TransactionOptions NoPay -> ChainCursor -> { _contractAddress :: Address, _tokenId :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 (Either CallError Address)
tokenCreator x0 cm r = uncurryFields  r $ tokenCreator' x0 cm
   where
    tokenCreator' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_contractAddress") Address) -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 (Either CallError Address)
    tokenCreator' y0 cm' y2 y3 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple2 y2 y3) :: TokenCreatorFn)