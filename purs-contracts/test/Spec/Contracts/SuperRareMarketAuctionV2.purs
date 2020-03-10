module Test.Spec.Contracts.SuperRareMarketAuctionV2 where

import Prelude
import Chanterelle.Internal.Deploy (DeployReceipt)
import Chanterelle.Internal.Logging (LogLevel(..), log)
import Chanterelle.Internal.Types (NoArgs)
import Chanterelle.Test (buildTestConfig)
import Contracts.SuperRareMarketAuctionV2 (buy, hasTokenBeenSold, markTokensAsSold, marketplaceFee, primarySaleFee, royaltyFee, setSalePrice, tokenPrice) as SuperRareMarketAuctionV2
import Contracts.SuperRareV2 as SuperRareV2
import Data.Array (catMaybes, elem, filter, length, replicate, take, zip, zipWith)
import Data.Either (Either(..), fromRight)
import Data.Lens ((?~))
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.Symbol (SProxy(..))
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Deploy.Contracts.SuperRareMarketAuctionV2 (deployScript) as SuperRareMarketAuctionV2
import Deploy.Contracts.SuperRareV2 (SuperRareV2) as SuperRareV2
import Deploy.Utils (awaitTxSuccessWeb3)
import Effect.Aff (Aff)
import Effect.Aff.AVar (put)
import Effect.Aff.Class (liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Network.Ethereum.Core.BigNumber (divide)
import Network.Ethereum.Web3 (Address, BigNumber, BlockNumber(..), ChainCursor(..), HexString, Provider, Szabo, Transaction(..), TransactionReceipt(..), UIntN, Value, Web3, _to, _value, embed, fromMinorUnit, mkValue, toMinorUnit, unIntN, unUIntN)
import Network.Ethereum.Web3.Api (eth_getBalance, eth_getTransaction, eth_getTransactionReceipt)
import Network.Ethereum.Web3.Solidity.Sizes (S256)
import Network.Ethereum.Web3.Types (class TokenUnit)
import Network.Ethereum.Web3.Types.TokenUnit (ProxyTU(..))
import Partial.Unsafe (unsafePartial)
import Prim.Row (class Lacks)
import Record as Record
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (randomSample')
import Test.Spec (SpecT, beforeAll, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Contracts.SuperRareV2 as SuperRareV2Spec
import Test.Spec.Contracts.Utils (defaultTxOpts, intToUInt256, readOrFail, throwOnCallError, uInt256FromBigNumber, web3Test)
import Type.Proxy (Proxy(..))

spec :: SpecT Aff Unit Aff Unit
spec =
  beforeAll init do
    describe "SuperRareMarketAuctionV2" do
      it "can mark tokens as sold" \tenv@{ provider } ->
        web3Test provider do
          newTokens <- mkSuperRareTokens tenv 4
          markTokensAsSold tenv (newTokens <#> \{ tokenId } -> tokenId)
          isMarkedSolds <-
            for (newTokens <#> \{ tokenId } -> tokenId) (hasTokenBeenSold tenv)
          isMarkedSolds `shouldEqual` replicate (length newTokens) true
      it "can set the price of tokens" \tenv@{ provider } ->
        web3Test provider do
          newTokens <- mkSuperRareTokens tenv 4
          prices <- genTokenPrices $ length newTokens
          let
            tokenDetails = zipWith (Record.insert (SProxy :: _ "price")) prices newTokens
          void
            $ for tokenDetails \td@{ tokenId, price, owner } -> do
                setSalePrice tenv owner tokenId price
          onChainPrices <-
            for tokenDetails \{ tokenId } -> tokenPrice tenv tokenId
          onChainPrices `shouldEqual` (tokenDetails <#> \{ price } -> price)

-- it "can make primary sales" do
--   provider <- readOrFail provAV
--   accounts <- readOrFail accsAV
--   v2SuperRare <- readOrFail v2SuperRareAV
--   v2Tokens <- readOrFail v2TokensAV
--   v2Marketplace <- readOrFail v2MarketplaceAV
--   primAcc <- readOrFail primAccAv
--   web3Test provider do
--     primarySaleTokensAndOwner <- do
--       primarySaleTokens <-
--         map catMaybes $ for v2Tokens
--           $ \_tokenId -> do
--               sold <-
--                 throwOnCallError
--                   $ SuperRareMarketAuctionV2.hasTokenBeenSold
--                       (defaultTxOpts primAcc # _to ?~ v2Marketplace.deployAddress)
--                       Latest
--                       { _originContract: v2SuperRare.deployAddress, _tokenId }
--               pure if not sold then Just _tokenId else Nothing
--       for primarySaleTokens
--         $ \tokenId ->
--             { tokenId, owner: _ }
--               <$> getV2TokenOwner v2SuperRare.deployAddress tokenId
--     pricesWithTokenIdsRes <- do
--       fee <-
--         throwOnCallError
--           $ SuperRareMarketAuctionV2.marketplaceFee
--               (defaultTxOpts primAcc # _to ?~ v2Marketplace.deployAddress)
--               Latest
--       primarySaleFee <-
--         throwOnCallError
--           $ SuperRareMarketAuctionV2.primarySaleFee
--               (defaultTxOpts primAcc # _to ?~ v2Marketplace.deployAddress)
--               Latest
--       for primarySaleTokensAndOwner
--         $ \{ tokenId, owner } -> do
--             price <-
--               throwOnCallError
--                 $ SuperRareMarketAuctionV2.tokenPrice
--                     (defaultTxOpts primAcc # _to ?~ v2Marketplace.deployAddress)
--                     Latest
--                     { _originContract: v2SuperRare.deployAddress
--                     , _tokenId: tokenId
--                     }
--             pure
--               { tokenId
--               , buyerFee: (unUIntN fee) * (unUIntN price) `divide` embed 100
--               , sellerFee: (unUIntN primarySaleFee) * (unUIntN price) `divide` embed 100
--               , price: unUIntN price
--               , owner
--               , originContract: v2SuperRare.deployAddress
--               , marketContract: v2Marketplace.deployAddress
--               }
--     let
--       noOwners acc =
--         not
--           (acc `elem` map (\{ owner } -> owner) pricesWithTokenIdsRes)
--       buyers = filter noOwners accounts
--       buyersWithToken =
--         zipWith
--           ( \buyer priceDetails ->
--               Record.insert (SProxy :: _ "buyer") buyer priceDetails
--           )
--           buyers
--           pricesWithTokenIdsRes
--     purchaseDetails <- for buyersWithToken buyTokenMarketV2
--     void $ for purchaseDetails checkNewOwnerStatus
--     void $ for purchaseDetails checkPayout
-- it "can make secondary sales" do
--   provider <- readOrFail provAV
--   accounts <- readOrFail accsAV
--   v2SuperRare <- readOrFail v2SuperRareAV
--   v2Tokens <- readOrFail v2TokensAV
--   v2Marketplace <- readOrFail v2MarketplaceAV
--   primAcc <- readOrFail primAccAv
--   web3Test provider do
--     purchasePayloads <-
--       map catMaybes
--         $ for v2Tokens
--             ( mkPurchasePayload
--                 false
--                 v2Marketplace.deployAddress
--                 v2SuperRare.deployAddress
--             )
--     let
--       noOwners acc =
--         not
--           (acc `elem` map (\{ owner } -> owner) purchasePayloads)
--       buyers = filter noOwners accounts
--       completePayloads =
--         zipWith (Record.insert (SProxy :: _ "buyer"))
--           buyers
--           purchasePayloads
--     log Info $ show { accounts, purchasePayloads, buyers }
--     purchaseDetails <- for completePayloads buyTokenMarketV2
--     void $ for purchaseDetails checkNewOwnerStatus
--     void $ for purchaseDetails checkPayout
-----------------------------------------------------------------------------
-- | TestEnv
-----------------------------------------------------------------------------
type TestEnv r
  = { supeRare :: DeployReceipt NoArgs
    , provider :: Provider
    , accounts :: Array Address
    , primaryAccount :: Address
    , v2SuperRare :: DeployReceipt SuperRareV2.SuperRareV2
    , v2Marketplace :: DeployReceipt NoArgs
    | r
    }

init :: Aff (TestEnv ())
init = do
  tenv@{ provider } <- initSupeRareV2
  { superRareMarketAuctionV2 } <-
    buildTestConfig "http://localhost:8545" 60
      SuperRareMarketAuctionV2.deployScript
  web3Test provider
    $ approveMarketplace tenv superRareMarketAuctionV2.deployAddress
  pure $ Record.insert (SProxy :: _ "v2Marketplace") superRareMarketAuctionV2 tenv
  where
  initSupeRareV2 = do
    tenv@{ accounts, provider } <- SuperRareV2Spec.init
    web3Test provider $ whitelistAddresses tenv
    pure tenv

  whitelistAddresses tenv@{ accounts } = void $ for accounts (SuperRareV2Spec.whitelistAddress tenv)

  approveMarketplace tenv@{ accounts } marketplace =
    void
      $ for accounts (\acc -> SuperRareV2Spec.setApprovalForAll tenv acc marketplace true)

setSalePrice ::
  forall r. TestEnv r -> Address -> UIntN S256 -> UIntN S256 -> Web3 Unit
setSalePrice tenv owner _tokenId _amount =
  let
    { v2SuperRare: { deployAddress: _originContract }
    , v2Marketplace: { deployAddress }
    , primaryAccount
    } = tenv
  in
    SuperRareMarketAuctionV2.setSalePrice
      (defaultTxOpts owner # _to ?~ deployAddress)
      { _originContract, _tokenId, _amount }
      >>= awaitTxSuccessWeb3

markTokensAsSold ::
  forall r. TestEnv r -> Array (UIntN S256) -> Web3 Unit
markTokensAsSold tenv _tokenIds =
  let
    { v2SuperRare: { deployAddress: _originContract }
    , v2Marketplace: { deployAddress }
    , primaryAccount
    } = tenv
  in
    SuperRareMarketAuctionV2.markTokensAsSold
      (defaultTxOpts primaryAccount # _to ?~ deployAddress)
      { _originContract, _tokenIds }
      >>= awaitTxSuccessWeb3

hasTokenBeenSold ::
  forall r. TestEnv r -> UIntN S256 -> Web3 Boolean
hasTokenBeenSold tenv _tokenId =
  let
    { v2SuperRare: { deployAddress: _originContract }
    , v2Marketplace: { deployAddress }
    , primaryAccount
    } = tenv
  in
    throwOnCallError
      $ SuperRareMarketAuctionV2.hasTokenBeenSold
          (defaultTxOpts primaryAccount # _to ?~ deployAddress)
          Latest
          { _originContract, _tokenId }

tokenPrice ::
  forall r. TestEnv r -> UIntN S256 -> Web3 (UIntN S256)
tokenPrice tenv _tokenId =
  let
    { v2SuperRare: { deployAddress: _originContract }
    , v2Marketplace: { deployAddress }
    , primaryAccount
    } = tenv
  in
    throwOnCallError
      $ SuperRareMarketAuctionV2.tokenPrice
          (defaultTxOpts primaryAccount # _to ?~ deployAddress)
          Latest
          { _originContract, _tokenId }

mkSuperRareTokens :: forall r. TestEnv r -> Int -> Web3 (Array { owner ∷ Address, tokenId ∷ UIntN S256, uri ∷ String })
mkSuperRareTokens tenv n =
  SuperRareV2Spec.createTokensWithFunction
    tenv
    n
    (SuperRareV2Spec.addNewToken tenv)

genTokenPrices :: forall m. MonadEffect m => Int -> m (Array (UIntN S256))
genTokenPrices n =
  liftEffect (randomSample' n arbitrary)
    >>= traverse \(intPrice :: Int) ->
        let
          (unitPrice :: Value Szabo) = mkValue $ embed $ abs intPrice
        in
          pure $ uInt256FromBigNumber $ toMinorUnit unitPrice

mkPurchasePayload ::
  Boolean ->
  Address ->
  Address ->
  UIntN S256 ->
  Web3
    ( Maybe
        { buyerFee :: BigNumber
        , owner :: Address
        , price :: BigNumber
        , sellerFee :: BigNumber
        , tokenId :: UIntN S256
        , originContract :: Address
        , marketContract :: Address
        }
    )
mkPurchasePayload primarySale marketAddr originAddr tokenId = do
  fee <-
    unUIntN
      <$> ( throwOnCallError
            $ SuperRareMarketAuctionV2.marketplaceFee
                (defaultTxOpts originAddr # _to ?~ marketAddr)
                Latest
        )
  sold <-
    throwOnCallError
      $ SuperRareMarketAuctionV2.hasTokenBeenSold
          (defaultTxOpts originAddr # _to ?~ marketAddr)
          Latest
          { _originContract: originAddr, _tokenId: tokenId }
  price <-
    unUIntN
      <$> ( throwOnCallError
            $ SuperRareMarketAuctionV2.tokenPrice
                (defaultTxOpts originAddr # _to ?~ marketAddr)
                Latest
                { _originContract: originAddr
                , _tokenId: tokenId
                }
        )
  if (not sold == primarySale) || (price == embed 0) then
    pure Nothing
  else do
    let
      getSellerFee =
        if primarySale then
          SuperRareMarketAuctionV2.primarySaleFee
        else
          SuperRareMarketAuctionV2.royaltyFee

      getSellerFee' =
        unUIntN
          <$> ( throwOnCallError
                $ getSellerFee
                    (defaultTxOpts originAddr # _to ?~ marketAddr)
                    Latest
            )
    pp <-
      { tokenId
      , marketContract: marketAddr
      , originContract: originAddr
      , buyerFee: fee
      , price
      , owner: _
      , sellerFee: _
      }
        <$> getV2TokenOwner originAddr tokenId
        <*> getSellerFee'
    log Info $ show pp
    pure $ Just pp

checkNewOwnerStatus ::
  forall r.
  { buyer :: Address
  , tokenId :: UIntN S256
  , originContract :: Address
  | r
  } ->
  Web3 Unit
checkNewOwnerStatus { buyer, tokenId, originContract } = do
  owner <- getV2TokenOwner originContract tokenId
  buyer `shouldEqual` owner

checkPayout ::
  forall r.
  { buyer :: Address
  , owner :: Address
  , buyerFee :: BigNumber
  , price :: BigNumber
  , purchaseTxHash :: HexString
  , sellerFee :: BigNumber
  | r
  } ->
  Web3 Unit
checkPayout { buyer, owner, purchaseTxHash, price, buyerFee, sellerFee } = do
  { blockNumber, gasUsed, gasPrice, from, to } <- getTxDetails purchaseTxHash
  let
    (BlockNumber blockNumBN) = blockNumber
  buyerBalanceAfter <- getBalance buyer $ BN blockNumber
  ownerBalanceAfter <- getBalance owner $ BN blockNumber
  buyerBalanceBefore <- getBalance buyer $ BN $ BlockNumber $ blockNumBN - embed 1
  ownerBalanceBefore <- getBalance owner $ BN $ BlockNumber $ blockNumBN - embed 1
  let
    weiSpentOnGas = gasPrice * gasUsed

    ownerBalanceDifference = abs $ ownerBalanceAfter - ownerBalanceBefore

    buyerBalanceDifference = abs $ buyerBalanceAfter - buyerBalanceBefore

    ownerEarned = price - sellerFee - if from == owner then weiSpentOnGas else embed 0

    buyerSpent = price + buyerFee + if from == buyer then weiSpentOnGas else embed 0
  ownerEarned `shouldEqual` ownerBalanceDifference
  buyerSpent `shouldEqual` buyerBalanceDifference
  where
  getBalance addr bn = eth_getBalance addr bn

  getTxDetails txHash = do
    (Transaction { gasPrice, from, to }) <- eth_getTransaction txHash
    (TransactionReceipt { blockNumber, gasUsed }) <- eth_getTransactionReceipt txHash
    pure { blockNumber, gasPrice, gasUsed, from, to }

buyTokenMarketV2 ::
  forall r.
  Lacks "purchaseTxHash" r =>
  { buyer :: Address
  , tokenId :: UIntN S256
  , owner :: Address
  , price :: BigNumber
  , buyerFee :: BigNumber
  , originContract :: Address
  , marketContract :: Address
  | r
  } ->
  Web3
    { buyer :: Address
    , tokenId :: UIntN S256
    , owner :: Address
    , price :: BigNumber
    , buyerFee :: BigNumber
    , originContract :: Address
    , marketContract :: Address
    , purchaseTxHash :: HexString
    | r
    }
buyTokenMarketV2 purchaseDetails@{ tokenId
, price
, buyerFee
, owner
, originContract
, buyer
, marketContract
} = do
  log Info
    $ ( show buyer
          <> " purchasing "
          <> show tokenId
          <> " from owner "
          <> show owner
          <> " for (fee included): "
          <> show (price + buyerFee)
      )
  txHash <-
    SuperRareMarketAuctionV2.buy
      ( defaultTxOpts buyer
          # _to
          ?~ marketContract
          # _value
          ?~ fromMinorUnit (price + buyerFee)
      )
      { _tokenId: tokenId, _originContract: originContract }
  awaitTxSuccessWeb3 txHash
  pure $ Record.insert (SProxy :: _ "purchaseTxHash") txHash purchaseDetails

getV2TokenOwner :: Address -> UIntN S256 -> Web3 Address
getV2TokenOwner caddr tokenId =
  throwOnCallError
    $ SuperRareV2.ownerOf
        (defaultTxOpts caddr # _to ?~ caddr)
        Latest
        { tokenId }
 {-
    deploy SR AuctionMarketV2
    -- 
    -- buy
    -- bid
    -- accept bid
    deploy hack
    -- make hack bid on V2 Marketplace
    -- outbid hack bid on V2 Marketplace
    -- 
    -- 
    -- 
        
        
-}