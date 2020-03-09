module Test.Spec.Contracts.SuperRareMarketAuctionV2 where

import Prelude
import Chanterelle.Test (buildTestConfig)
import Contracts.SuperRareMarketAuctionV2 (buy, hasTokenBeenSold, markTokensAsSold, setSalePrice, tokenPrice) as SuperRareMarketAuctionV2
import Contracts.SuperRareV2 as SuperRareV2
import Data.Array (catMaybes, elem, filter, length, replicate, take, zip, zipWith)
import Data.Array.Partial (head)
import Data.Either (Either(..), fromRight)
import Data.Lens ((?~))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Deploy.Contracts.SuperRareMarketAuctionV2 (deployScript) as SuperRareMarketAuctionV2
import Deploy.Utils (awaitTxSuccessWeb3)
import Effect.Aff (Aff, error)
import Effect.Aff.AVar (put)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throwException)
import Network.Ethereum.Web3 (Address, ChainCursor(..), HexString, UIntN, Web3, _to, _value, fromMinorUnit, runWeb3, unUIntN)
import Network.Ethereum.Web3.Solidity.Sizes (S256)
import Partial.Unsafe (unsafePartial)
import Record as Record
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (randomSample')
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Contracts.Utils (TestEnv, defaultTxOpts, intToUInt256, readOrFail)

spec :: TestEnv -> SpecT Aff Unit Aff Unit
spec { superRareMarketAuctionV2: v2MarketplaceAV
, accounts: accsAV
, provider: provAV
, superRareV2: v2SuperRareAV
, superRareV2Tokens: v2TokensAV
} = do
  describe "SuperRareMarketAuctionV2" do
    it "can deploy the contract" do
      v2Marketplace <-
        liftAff $ buildTestConfig "http://localhost:8545" 60 SuperRareMarketAuctionV2.deployScript
      put v2Marketplace.superRareMarketAuctionV2 v2MarketplaceAV
    it "can mark tokens as sold" do
      provider <- readOrFail provAV
      accounts <- readOrFail accsAV
      v2SuperRare <- readOrFail v2SuperRareAV
      v2Tokens <- readOrFail v2TokensAV
      superRareMarketAuctionV2@{ deployAddress } <- readOrFail v2MarketplaceAV
      let
        runWeb3' = liftAff <<< runWeb3 provider

        acc1 = unsafePartial $ head accounts

        soldTokens = take 2 v2Tokens
      void
        $ runWeb3' do
            SuperRareMarketAuctionV2.markTokensAsSold
              (defaultTxOpts acc1 # _to ?~ deployAddress)
              { _tokenIds: soldTokens, _originContract: v2SuperRare.deployAddress }
              >>= awaitTxSuccessWeb3
            isMarkedSolds <-
              traverse
                ( \_tokenId ->
                    SuperRareMarketAuctionV2.hasTokenBeenSold
                      (defaultTxOpts acc1 # _to ?~ deployAddress)
                      Latest
                      { _originContract: v2SuperRare.deployAddress, _tokenId }
                )
                soldTokens
            isMarkedSolds `shouldEqual` replicate (length soldTokens) (Right true)
    it "can approve the marketplace for new tokens" do
      provider <- readOrFail provAV
      accounts <- readOrFail accsAV
      v2SuperRare <- readOrFail v2SuperRareAV
      v2Tokens <- readOrFail v2TokensAV
      v2Marketplace <- readOrFail v2MarketplaceAV
      superRareMarketAuctionV2 <- readOrFail v2MarketplaceAV
      let
        runWeb3' = liftAff <<< runWeb3 provider

        acc1 = unsafePartial $ head accounts

        soldTokens = take 2 v2Tokens
      void
        $ runWeb3' do
            void $ for accounts
              $ \acc ->
                  SuperRareV2.setApprovalForAll
                    (defaultTxOpts acc # _to ?~ v2SuperRare.deployAddress)
                    { approved: true, to: v2Marketplace.deployAddress }
                    >>= awaitTxSuccessWeb3
            isApprovedForAlls <-
              for accounts
                $ \owner ->
                    SuperRareV2.isApprovedForAll
                      (defaultTxOpts acc1 # _to ?~ v2SuperRare.deployAddress)
                      Latest
                      { operator: v2Marketplace.deployAddress, owner }
            isApprovedForAlls `shouldEqual` replicate (length accounts) (Right true)
    it "can set the price of tokens" do
      provider <- readOrFail provAV
      accounts <- readOrFail accsAV
      v2SuperRare <- readOrFail v2SuperRareAV
      v2Tokens <- readOrFail v2TokensAV
      v2Marketplace <- readOrFail v2MarketplaceAV
      let
        runWeb3' = liftAff <<< runWeb3 provider

        acc1 = unsafePartial $ head accounts
      prices <- map (map intToUInt256) $ liftEffect $ randomSample' (length v2Tokens) arbitrary
      log $ show prices
      let
        pricesWithTokenIds = zip v2Tokens prices
      void
        $ runWeb3' do
            void $ for pricesWithTokenIds
              $ \(Tuple _tokenId _amount) ->
                  do
                    owner <-
                      map (unsafePartial fromRight)
                        $ SuperRareV2.ownerOf
                            (defaultTxOpts acc1 # _to ?~ v2SuperRare.deployAddress)
                            Latest
                            { tokenId: _tokenId }
                    SuperRareMarketAuctionV2.setSalePrice
                      (defaultTxOpts owner # _to ?~ v2Marketplace.deployAddress)
                      { _amount, _originContract: v2SuperRare.deployAddress, _tokenId }
                    >>= awaitTxSuccessWeb3
            pricesWithTokenIdsRes <-
              for v2Tokens
                $ \_tokenId -> do
                    amount <-
                      map (unsafePartial fromRight)
                        $ SuperRareMarketAuctionV2.tokenPrice
                            (defaultTxOpts acc1 # _to ?~ v2Marketplace.deployAddress)
                            Latest
                            { _originContract: v2SuperRare.deployAddress, _tokenId }
                    pure (Tuple _tokenId amount)
            pricesWithTokenIdsRes `shouldEqual` pricesWithTokenIds
    it "can make primary sale" do
      provider <- readOrFail provAV
      accounts <- readOrFail accsAV
      v2SuperRare <- readOrFail v2SuperRareAV
      v2Tokens <- readOrFail v2TokensAV
      v2Marketplace <- readOrFail v2MarketplaceAV
      let
        runWeb3' = liftAff <<< runWeb3 provider

        acc1 = unsafePartial $ head accounts
      void
        $ runWeb3' do
            primarySaleTokensAndOwner <- do
              primarySaleTokens <-
                map catMaybes $ for v2Tokens
                  $ \_tokenId -> do
                      sold <-
                        map (unsafePartial fromRight)
                          $ SuperRareMarketAuctionV2.hasTokenBeenSold
                              (defaultTxOpts acc1 # _to ?~ v2Marketplace.deployAddress)
                              Latest
                              { _originContract: v2SuperRare.deployAddress, _tokenId }
                      pure if not sold then Just _tokenId else Nothing
              for primarySaleTokens
                $ \tokenId ->
                    { tokenId, owner: _ }
                      <$> getV2TokenOwner v2SuperRare.deployAddress tokenId
            -- for primarySaleTokens $ \ tokenId
            pricesWithTokenIdsRes <-
              for primarySaleTokensAndOwner
                $ \{ tokenId, owner } -> do
                    price <-
                      map (unsafePartial fromRight)
                        $ SuperRareMarketAuctionV2.tokenPrice
                            (defaultTxOpts acc1 # _to ?~ v2Marketplace.deployAddress)
                            Latest
                            { _originContract: v2SuperRare.deployAddress, _tokenId: tokenId }
                    pure { tokenId, price, owner, originContract: v2SuperRare.deployAddress }
            let
              buyers = filter (\acc -> not (acc `elem` map (\{ owner } -> owner) pricesWithTokenIdsRes)) accounts

              buyersWithToken = zipWith (\buyer priceDetails -> Record.insert (SProxy :: _ "buyer") buyer priceDetails) buyers pricesWithTokenIdsRes
            purchaseDetails <- for buyersWithToken buyTokenMarketV2
            for purchaseDetails checkNewOwnerStatus
            for purchaseDetails checkPayout

checkNewOwnerStatus :: forall r. { buyer :: Address, tokenId :: UIntN S256, originContract :: Address | r } -> Web3 Unit
checkNewOwnerStatus { buyer, tokenId, originContract } = do
  owner <- getV2TokenOwner originContract tokenId
  buyer `shouldEqual` owner

checkPayout :: { buyer :: Address, owner :: Address, price :: UIntN S256, originContract :: Address, purchaseTxHash :: HexString } -> Web3 Unit
checkPayout = ?todo

buyTokenMarketV2 ::
  { buyer :: Address, tokenId :: UIntN S256, owner :: Address, price :: UIntN S256, originContract :: Address } ->
  Web3 { buyer :: Address, tokenId :: UIntN S256, owner :: Address, price :: UIntN S256, originContract :: Address, purchaseTxHash :: HexString }
buyTokenMarketV2 purchaseDetails { tokenId, price, owner, originContract, buyer } = do
  txHash <-
    SuperRareMarketAuctionV2.buy
      (defaultTxOpts buyer # _to ?~ originContract # _value ?~ fromMinorUnit (unUIntN price))
      { _tokenId: tokenId, _originContract: originContract }
  pure Record.insert (SProxy :: _ "purchaseTxHash") txHash purchaseDetails

getV2TokenOwner :: Address -> UIntN S256 -> Web3 Address
getV2TokenOwner caddr tokenId = do
  eo <-
    SuperRareV2.ownerOf
      (defaultTxOpts caddr # _to ?~ caddr)
      Latest
      { tokenId }
  case eo of
    Left err -> liftEffect $ throwException $ error $ show err
    Right addr -> pure addr
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