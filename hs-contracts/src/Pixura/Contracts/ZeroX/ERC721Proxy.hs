{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

module Pixura.Contracts.ZeroX.ERC721Proxy where

import           Network.Ethereum.Contract.TH

[abiFrom|contracts/tuple-abis/ERC721Proxy.json|]
