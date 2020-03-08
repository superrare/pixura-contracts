module Test.Spec.Contracts.Utils where

import Prelude
import Chanterelle.Internal.Deploy (DeployReceipt)
import Chanterelle.Internal.Types (NoArgs)
import Deploy.Contracts.SuperRareV2 as SuperRareV2
import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar, empty)
import Effect.Aff.Class (liftAff)
import Network.Ethereum.Web3 (Address, Provider, UIntN)
import Network.Ethereum.Web3.Solidity.Sizes (S256)

type TestEnv
  = { supeRare :: AVar (DeployReceipt NoArgs)
    , superRareV2 :: AVar (DeployReceipt SuperRareV2.SuperRareV2)
    , superRareMarketAuction :: AVar (DeployReceipt NoArgs)
    , superRareMarketAuctionV2 :: AVar (DeployReceipt NoArgs)
    , supeRareTokens :: AVar (Array (UIntN S256))
    , superRareV2Tokens :: AVar (Array (UIntN S256))
    , provider :: AVar Provider
    , accounts :: AVar (Array Address)
    }

init :: Aff TestEnv
init =
  { supeRare: _
  , superRareV2: _
  , superRareMarketAuction: _
  , superRareMarketAuctionV2: _
  , supeRareTokens: _
  , superRareV2Tokens: _
  , provider: _
  , accounts: _
  }
    <$> liftAff empty
    <*> liftAff empty
    <*> liftAff empty
    <*> liftAff empty
    <*> liftAff empty
    <*> liftAff empty
    <*> liftAff empty
    <*> liftAff empty
