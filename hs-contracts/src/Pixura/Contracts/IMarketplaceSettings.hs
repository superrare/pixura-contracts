{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

module Pixura.Contracts.IMarketplaceSettings where

import           Network.Ethereum.Contract.TH

[abiFrom|contracts/build/Marketplace/IMarketplaceSettings.json|]
