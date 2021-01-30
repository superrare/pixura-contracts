--------------------------------------------------------------------------------
-- | TestAssertFailOnPay
--------------------------------------------------------------------------------

module Contracts.TestAssertFailOnPay where

import Prelude 

import Data.Functor.Tagged (Tagged, tagged)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (D2, D5, D6, DOne, Tuple4(..), UIntN)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, HexString, TransactionOptions, Web3)
import Network.Ethereum.Web3.Types.TokenUnit (MinorUnit)


--------------------------------------------------------------------------------
-- | BidFn
--------------------------------------------------------------------------------


type BidFn = Tagged (SProxy "bid(uint256,address,uint256,address)") (Tuple4 (Tagged (SProxy "_newBidAmount") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "_originContract") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "_market") Address))

bid :: TransactionOptions MinorUnit -> { _newBidAmount :: (UIntN (D2 :& D5 :& DOne D6)), _originContract :: Address, _tokenId :: (UIntN (D2 :& D5 :& DOne D6)), _market :: Address } -> Web3 HexString
bid x0 r = uncurryFields  r $ bid' x0
   where
    bid' :: TransactionOptions MinorUnit -> (Tagged (SProxy "_newBidAmount") (UIntN (D2 :& D5 :& DOne D6))) -> (Tagged (SProxy "_originContract") Address) -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> (Tagged (SProxy "_market") Address) -> Web3 HexString
    bid' y0 y1 y2 y3 y4 = sendTx y0 ((tagged $ Tuple4 y1 y2 y3 y4) :: BidFn)