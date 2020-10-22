module Test.Spec.Contracts.Utils where

import Prelude
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Array (length, zipWith)
import Data.Array.NonEmpty (NonEmptyArray, fromArray, toNonEmpty)
import Data.Either (Either(..))
import Data.Lens ((?~))
import Data.Maybe (Maybe(..), fromJust)
import Data.NonEmpty (NonEmpty(..))
import Data.Traversable (for)
import Effect.Aff (Error, error)
import Effect.Aff.AVar (AVar, tryRead)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (logShow)
import Effect.Exception.Unsafe (unsafeThrow)
import Network.Ethereum.Core.BigNumber (BigNumber, decimal, embed, parseBigNumber)
import Network.Ethereum.Core.HexString (nullWord, takeHex)
import Network.Ethereum.Web3 (class KnownSize, Address, CallError, DLProxy(..), Provider, TransactionOptions, UIntN, Web3, _from, _gas, _gasPrice, defaultTransactionOptions, mkAddress, runWeb3, uIntNFromBigNumber)
import Network.Ethereum.Web3.Solidity.Sizes (S256, s256)
import Network.Ethereum.Web3.Types (NoPay)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (Gen, elements, randomSample')
import Test.Spec.Assertions (shouldEqual)

readOrFail :: forall m a. (MonadAff m) => AVar a -> m a
readOrFail x = do
  mx <- liftAff $ tryRead x
  pure $ unsafePartial $ fromJust mx

mkTokenUris :: forall m. MonadEffect m => Int -> m (Array String)
mkTokenUris n = map (map show) $ liftEffect $ randomSample' n (arbitrary :: Gen Int)

defaultTxOpts :: Address -> TransactionOptions NoPay
defaultTxOpts primaryAccount =
  let
    limit = unsafePartial fromJust $ parseBigNumber decimal "6712388"

    price = unsafePartial fromJust $ parseBigNumber decimal "10000000"
  in
    defaultTransactionOptions # _from ?~ primaryAccount
      # _gas
      ?~ limit
      # _gasPrice
      ?~ price

uInt256FromBigNumber :: BigNumber -> UIntN S256
uInt256FromBigNumber n = case uIntNFromBigNumber s256 n of
  Nothing -> (unsafeThrow $ "Invalid BigNumber " <> show n <> " to build UInt256")
  Just n' -> n'

intToUInt256 :: Int -> UIntN S256
intToUInt256 = uInt256FromBigNumber <<< embed

unSafeUIntNFromBigNumber :: forall n. KnownSize n => DLProxy n -> BigNumber -> UIntN n
unSafeUIntNFromBigNumber s n = case uIntNFromBigNumber s n of
  Nothing -> (unsafeThrow $ "Invalid BigNumber " <> show n <> " to build UIntN")
  Just n' -> n'

intToUIntN :: forall n. KnownSize n => DLProxy n -> Int -> UIntN n
intToUIntN s = unSafeUIntNFromBigNumber s <<< embed

web3Test ::
  forall m.
  MonadAff m =>
  MonadThrow Error m => Provider -> Web3 Unit -> m Unit
web3Test prov f = do
  res <- liftAff $ runWeb3 prov f
  res `shouldEqual` Right unit

throwOnCallError :: forall a m. MonadThrow Error m => m (Either CallError a) -> m a
throwOnCallError f =
  f
    >>= case _ of
        Left cerr -> throwError $ error $ show cerr
        Right x -> pure x

createTokensWithFunction ::
  forall r.
  { accounts :: Array Address | r } ->
  Int ->
  ( Address ->
    String ->
    Web3
      ( { tokenId :: UIntN S256, contractAddress :: Address }
      )
  ) ->
  Web3
    ( Array
        { tokenId :: UIntN S256
        , owner :: Address
        , uri :: String
        , contractAddress :: Address
        }
    )
createTokensWithFunction { accounts } amount f = do
  let
    accounts' = unsafePartial fromJust $ (toNonEmpty <$> fromArray accounts)
  tokenUris <- mkTokenUris amount
  accs <- liftEffect $ randomSample' amount (elements accounts')
  for (zipWith { acc: _, _uri: _ } accs tokenUris) \{ acc, _uri } -> do
    { tokenId, contractAddress } <- f acc _uri
    pure { owner: acc, uri: _uri, tokenId, contractAddress }

nullAddress :: Address
nullAddress = unsafePartial fromJust $ mkAddress $ takeHex 40 nullWord
