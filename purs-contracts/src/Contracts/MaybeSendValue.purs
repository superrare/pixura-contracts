--------------------------------------------------------------------------------
-- | MaybeSendValue
--------------------------------------------------------------------------------

module Contracts.MaybeSendValue where

import Prelude 

import Data.Functor.Tagged (Tagged, tagged)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (deployContract)
import Network.Ethereum.Web3.Solidity (Tuple0(..))
import Network.Ethereum.Web3.Types (HexString, NoPay, TransactionOptions, Web3)
--------------------------------------------------------------------------------
-- | ConstructorFn
--------------------------------------------------------------------------------


type ConstructorFn = Tagged (SProxy "constructor()") (Tuple0 )

constructor :: TransactionOptions NoPay -> HexString -> Web3 HexString
constructor x0 bc = deployContract x0 bc ((tagged $ Tuple0 ) :: ConstructorFn)