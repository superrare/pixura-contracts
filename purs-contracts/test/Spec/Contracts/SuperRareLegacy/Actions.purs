module Test.Spec.Contracts.SuperRareLegacy.Actions where

import Prelude
import Chanterelle.Internal.Deploy (DeployReceipt)
import Chanterelle.Internal.Types (NoArgs)
import Contracts.SupeRare as SupeRare
import Contracts.SuperRareLegacy as SuperRareLegacy
import Data.Lens ((?~))
import Deploy.Contracts.SuperRareLegacy (SuperRareLegacy)
import Deploy.Utils (awaitTxSuccessWeb3)
import Network.Ethereum.Web3 (Address, ChainCursor(..), HexString, Provider, UIntN, Web3, _to)
import Network.Ethereum.Web3.Solidity.Sizes (S256)
import Test.Spec.Contracts.Utils (defaultTxOpts, throwOnCallError)

-----------------------------------------------------------------------------
-- | TestEnv
-----------------------------------------------------------------------------
type TestEnv r
  = { supeRare :: DeployReceipt NoArgs
    , superRareLegacy :: DeployReceipt SuperRareLegacy
    , provider :: Provider
    , accounts :: Array Address
    , primaryAccount :: Address
    , numOldSuperRareTokens :: Int
    | r
    }

-----------------------------------------------------------------------------
-- | totalSupply
-----------------------------------------------------------------------------
totalSupply ::
  forall r. TestEnv r -> Web3 (UIntN S256)
totalSupply tenv =
  let
    { superRareLegacy: { deployAddress }
    , primaryAccount
    } = tenv
  in
    throwOnCallError
      $ SuperRareLegacy.totalSupply
          (defaultTxOpts primaryAccount # _to ?~ deployAddress)
          Latest

-----------------------------------------------------------------------------
-- | preUpgradeOwnerOf
-----------------------------------------------------------------------------
preUpgradeOwnerOf ::
  forall r. TestEnv r -> UIntN S256 -> Web3 Address
preUpgradeOwnerOf tenv _tokenId =
  let
    { superRareLegacy: { deployAddress }
    , primaryAccount
    } = tenv
  in
    throwOnCallError
      $ SuperRareLegacy.preUpgradeOwnerOf
          (defaultTxOpts primaryAccount # _to ?~ deployAddress)
          Latest
          { _tokenId }

-----------------------------------------------------------------------------
-- | ownerOf
-----------------------------------------------------------------------------
ownerOf ::
  forall r. TestEnv r -> UIntN S256 -> Web3 Address
ownerOf tenv _tokenId =
  let
    { superRareLegacy: { deployAddress }
    , primaryAccount
    } = tenv
  in
    throwOnCallError
      $ SuperRareLegacy.ownerOf
          (defaultTxOpts primaryAccount # _to ?~ deployAddress)
          Latest
          { _tokenId }

-----------------------------------------------------------------------------
-- | tokenURI
-----------------------------------------------------------------------------
tokenURI ::
  forall r. TestEnv r -> UIntN S256 -> Web3 String
tokenURI tenv tokenId =
  let
    { superRareLegacy: { deployAddress }
    , primaryAccount
    } = tenv
  in
    throwOnCallError
      $ SuperRareLegacy.tokenURI
          (defaultTxOpts primaryAccount # _to ?~ deployAddress)
          Latest
          { tokenId }

-----------------------------------------------------------------------------
-- | isUpgraded
-----------------------------------------------------------------------------
isUpgraded ::
  forall r. TestEnv r -> UIntN S256 -> Web3 Boolean
isUpgraded tenv _tokenId =
  let
    { superRareLegacy: { deployAddress }
    , primaryAccount
    } = tenv
  in
    throwOnCallError
      $ SuperRareLegacy.isUpgraded
          (defaultTxOpts primaryAccount # _to ?~ deployAddress)
          Latest
          { _tokenId }

-----------------------------------------------------------------------------
-- | refreshPreUpgradeOwnerOf
-----------------------------------------------------------------------------
refreshPreUpgradeOwnerOf ::
  forall r. TestEnv r -> UIntN S256 -> Web3 HexString
refreshPreUpgradeOwnerOf tenv _tokenId = do
  let
    { superRareLegacy: { deployAddress }
    , primaryAccount
    } = tenv
  txHash <-
    SuperRareLegacy.refreshPreUpgradeOwnerOf
      ( defaultTxOpts primaryAccount
          # _to
          ?~ deployAddress
      )
      { _tokenId }
  awaitTxSuccessWeb3 txHash
  pure txHash

-----------------------------------------------------------------------------
-- | tokenCreator
-----------------------------------------------------------------------------
tokenCreator ::
  forall r. TestEnv r -> UIntN S256 -> Web3 Address
tokenCreator tenv _tokenId =
  let
    { superRareLegacy: { deployAddress }
    , primaryAccount
    } = tenv
  in
    throwOnCallError
      $ SuperRareLegacy.tokenCreator
          (defaultTxOpts primaryAccount # _to ?~ deployAddress)
          Latest
          { _tokenId }

-----------------------------------------------------------------------------
-- | setApprovalForAll
-----------------------------------------------------------------------------
setApprovalForAll ::
  forall r. TestEnv r -> Address -> Address -> Boolean -> Web3 HexString
setApprovalForAll tenv owner operator approved = do
  let
    { superRareLegacy: { deployAddress }
    , primaryAccount
    } = tenv
  txHash <-
    SuperRareLegacy.setApprovalForAll
      ( defaultTxOpts owner
          # _to
          ?~ deployAddress
      )
      { approved, operator }
  awaitTxSuccessWeb3 txHash
  pure txHash

-----------------------------------------------------------------------------
-- | isApprovedForAll
-----------------------------------------------------------------------------
isApprovedForAll ::
  forall r. TestEnv r -> Address -> Address -> Web3 Boolean
isApprovedForAll tenv owner operator =
  let
    { superRareLegacy: { deployAddress }
    , primaryAccount
    } = tenv
  in
    throwOnCallError
      $ SuperRareLegacy.isApprovedForAll
          (defaultTxOpts primaryAccount # _to ?~ deployAddress)
          Latest
          { operator, owner }

-----------------------------------------------------------------------------
-- | transferFrom
-----------------------------------------------------------------------------
transferFrom ::
  forall r. TestEnv r -> Address -> Address -> UIntN S256 -> Web3 HexString
transferFrom tenv from to tokenId = do
  let
    { superRareLegacy: { deployAddress }
    , primaryAccount
    } = tenv
  txHash <-
    SuperRareLegacy.transferFrom
      ( defaultTxOpts from
          # _to
          ?~ deployAddress
      )
      { from, to, tokenId }
  awaitTxSuccessWeb3 txHash
  pure txHash

-----------------------------------------------------------------------------
-- | upgrade
-----------------------------------------------------------------------------
upgrade ::
  forall r. TestEnv r -> Address -> UIntN S256 -> Web3 HexString
upgrade tenv owner tokenId = do
  let
    { superRareLegacy: { deployAddress }
    , supeRare: { deployAddress: supeRareAddress }
    , primaryAccount
    } = tenv
  txHash <-
    SupeRare.transfer
      ( defaultTxOpts owner
          # _to
          ?~ supeRareAddress
      )
      { _to: deployAddress, _tokenId: tokenId }
  awaitTxSuccessWeb3 txHash
  pure txHash
