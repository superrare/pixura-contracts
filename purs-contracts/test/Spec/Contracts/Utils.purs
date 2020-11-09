module Test.Spec.Contracts.Utils where

import Prelude
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Array (concat, replicate, take, zipWith)
import Data.Array.NonEmpty (fromArray, toNonEmpty)
import Data.Array.Partial (head)
import Data.Either (Either(..))
import Data.Lens ((?~))
import Data.Maybe (Maybe(..), fromJust)
import Data.Traversable (for)
import Effect.Aff (Error, error)
import Effect.Aff.AVar (AVar, tryRead)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception.Unsafe (unsafeThrow)
import Network.Ethereum.Core.BigNumber (BigNumber, decimal, embed, parseBigNumber)
import Network.Ethereum.Core.HexString (nullWord, takeHex)
import Network.Ethereum.Web3 (class KnownSize, Address, CallError, DLProxy, Provider, TransactionOptions, UIntN, Web3, _from, _gas, _gasPrice, defaultTransactionOptions, mkAddress, runWeb3, uIntNFromBigNumber)
import Network.Ethereum.Web3.Solidity.Sizes (S256, s256)
import Network.Ethereum.Web3.Types (NoPay)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck.Gen (elements, randomSample', shuffle)
import Test.Spec.Assertions (shouldEqual)

readOrFail :: forall m a. (MonadAff m) => AVar a -> m a
readOrFail x = do
  mx <- liftAff $ tryRead x
  pure $ unsafePartial $ fromJust mx

mkTokenUris :: forall m. MonadEffect m => Int -> m (Array String)
mkTokenUris n = (take n <<< concat) <$> (liftEffect $ randomSample' 1 $ shuffle uris)
  where
  uris =
    [ "https://ipfs.pixura.io/ipfs/QmWG33vM4Arv5NEydJQxsWcjyLQdccDck1gPtagddckPzR/metadata.json"
    , "https://ipfs.pixura.io/ipfs/QmUgGyw16ozPCChY3gtmvcsmiqw11fEpisvQdDuxNQ9rY8/metadata.json"
    , "https://ipfs.pixura.io/ipfs/QmNdatpC7iVfF1KJtX1EYiTUNG7U8rQ6Zg6jHJyZDRhMrj/metadata.json"
    , "https://ipfs.pixura.io/ipfs/QmcJ7pTaq4A4M5XAmEeAjmkB5UmvfMwsRpSTabhHGdy9Li/metadata.json"
    , "https://ipfs.pixura.io/ipfs/QmNXidLxySSLnuKuLmjHHXPENGmxJeDbYjc3XmMmnmDxS6/metadata.json"
    , "https://ipfs.pixura.io/ipfs/QmYyUuFYkRtJRTcTSwzkXgbMVA5KFdyVVGMS9KEnE9ymmf/metadata.json"
    , "https://ipfs.pixura.io/ipfs/Qme7di6esFm8ExTRStDPCBP4hT1Cs4YERSBp3eHsnsSFYr/metadata.json"
    , "https://ipfs.pixura.io/ipfs/QmYd8Zmt4QmiT9PqDchpzMENNz9bGcvBGLRfeV9FhBi6gH/metadata.json"
    , "https://ipfs.pixura.io/ipfs/QmTnohPTwwKAy65n1oF2bMM9qoXb6N5fWjyBt4fbg6Mhyr/metadata.json"
    , "https://ipfs.pixura.io/ipfs/QmQH2fgax4EaxsD4CcuVz5xu9kZEktMGy1Kjijnojcbz4Q/metadata.json"
    , "https://ipfs.pixura.io/ipfs/QmPM95FVZ9yJ5NFttcz21R6c135LvvDbEsCN7tKBVqVJon/metadata.json"
    , "https://ipfs.pixura.io/ipfs/QmU1wrBs2tnxy9cGeTz8L1jTWspRjUv5pgdZmB3Gfi7YGr/metadata.json"
    , "https://ipfs.pixura.io/ipfs/QmSN4wkGm7Wk394yLs6gQUDv7uNnWNqZMgfZKVBryBhYwi/metadata.json"
    , "https://ipfs.pixura.io/ipfs/QmZwmAAxo4mJbi74CawBEy5LLv4YhS4YyAoe1jL1xicJg7/metadata.json"
    , "https://ipfs.pixura.io/ipfs/QmdqQWgNfCP1JtR8GGVtArcGWZwhjgVqgpH7QBRdwhWAMF/metadata.json"
    , "https://ipfs.pixura.io/ipfs/QmSZAGjGep1vUqH95X7jKEjyih974aGRANYuEZExPY522B/metadata.json"
    , "https://ipfs.pixura.io/ipfs/QmUBxZbsgCGbAxDsMnoh2Z6NGMEALSwkbktmzXR4646sZW/metadata.json"
    , "https://ipfs.pixura.io/ipfs/QmTJrwqwSEzgkdjPnSwZmHWkEm6ZGdtkpSPet5QPM1VaiA/metadata.json"
    , "https://ipfs.pixura.io/ipfs/QmSGu414gBPCxv2Lqfcb7u8vH7qH4mzaD8pXarQoHsEoED/metadata.json"
    , "https://ipfs.pixura.io/ipfs/QmSgev8Y9bPT2uXTyWPb6M1xhXiAp6nosR6tzJZxbPB3rC/metadata.json"
    , "https://ipfs.pixura.io/ipfs/QmTZqiWfETbApbVrCbavQ5RFjKoekrtv2SGjkFMJYfo7No/metadata.json"
    , "https://ipfs.pixura.io/ipfs/QmNNEXBZA7AtPRvBbkgC15c9DGQJ1zWDbXQw5N62JTXB9f/metadata.json"
    , "https://ipfs.pixura.io/ipfs/QmR3j4XPPeFqF8ZKzD6xFspW2rQqUd93CWWRpNypEzkcUT/metadata.json"
    , "https://ipfs.pixura.io/ipfs/QmbnCWuqxcsAGqGYSQ1GgyeS2SmX5MTpyzqY1S31ixdcT4/metadata.json"
    , "https://ipfs.pixura.io/ipfs/QmU7KUuG36j1Ag6jy1mtzMVQbm1M5Jh2sgjjPsAYV8K6SJ/metadata.json"
    , "https://ipfs.pixura.io/ipfs/QmZGy7qng6H4Hwzy7eJ8h7pXdVhNvtSVxQLKUcMz41e2Ug/metadata.json"
    , "https://ipfs.pixura.io/ipfs/Qmep566Ut79EgSJe7DW25NrAVHdJK1BePHCiJ3TYTwyshh/metadata.json"
    , "https://ipfs.pixura.io/ipfs/QmXM9ZpKs6L989dmcX2G1XfUco4nu3dtrh8vqS2Jb6pN6w/metadata.json"
    , "https://ipfs.pixura.io/ipfs/QmPdi2FpzPdv5P3JYfdqTc4eJhmHcz589jW8B85oaBpg74/metadata.json"
    , "https://ipfs.pixura.io/ipfs/QmbXcsQCGChpj9eER4qjYgP8MMrhL2xHAFYXjArmaFEwjH/metadata.json"
    , "https://ipfs.pixura.io/ipfs/Qmd7VeRFR3Amds5783Xe3EF6kMv2SCzXN1A2fb1XUvG54U/metadata.json"
    , "https://ipfs.pixura.io/ipfs/QmW2kgLNgZaaatZ3NK2JKYMFdtMs61cBxTxgwebtWHeBHH/metadata.json"
    , "https://ipfs.pixura.io/ipfs/QmX2syv6jdgBB4uj3cGDejErcZ6fwtUbwTgPWCtVm65qBL/metadata.json"
    , "https://ipfs.pixura.io/ipfs/QmTSC3pWP26gytbESuhhkQ53v67LiJFVAVaVhr25gFhqdD/metadata.json"
    , "https://ipfs.pixura.io/ipfs/QmdXiUh1PP5eAUxUFgYP8hrxYTpzpkAkCLp1HG7rzGqvY6/metadata.json"
    , "https://ipfs.pixura.io/ipfs/QmcVUifv9YxUkBGwkyA4VKeitMtSmQ6rUW8V6dtXtgLHoq/metadata.json"
    , "https://ipfs.pixura.io/ipfs/QmNs6szUwpECQZydABt1edJecYqPcGqs89zxEC2vABtyM2/metadata.json"
    , "https://ipfs.pixura.io/ipfs/QmPymyKkdYznnpPpeAYjEooMnRnui2Qi8L6kNG7A32W433/metadata.json"
    , "https://ipfs.pixura.io/ipfs/QmQchChMXQPSv9ukLEXtojEDn6GeaF6zBDMv7BhKqNk1gN/metadata.json"
    , "https://ipfs.pixura.io/ipfs/QmU2uEZ8TPR5xiJivsb9ivUhcFe8y6mPX7s7ZLSG99j5mk/metadata.json"
    , "https://ipfs.pixura.io/ipfs/QmU3tg18au8sk5oSSkK1rRxgqSH6B3aspQAb6WSF8qHUMb/metadata.json"
    , "https://ipfs.pixura.io/ipfs/Qmc2b8MwtGYcNW2LKnQESWTcpZzSMBWkT3En7E55nt4NRR/metadata.json"
    , "https://ipfs.pixura.io/ipfs/QmWi3kJbGp6RTDKbnrD52U8c4xiQt94iSebFXgDANgXKKT/metadata.json"
    , "https://ipfs.pixura.io/ipfs/QmNvcwRskPoYr7B6xrAs7x2PuoRUTbYodhtQsrtPkNw8on/metadata.json"
    , "https://ipfs.pixura.io/ipfs/QmWQgGx9xp9vQMJp5YSyiyTQYxWoyk2cPz9MG9PoysYL37/metadata.json"
    ]

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
