{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

module Pixura.Contracts.SuperRareMarketAuctionV2 where

import           Network.Ethereum.Contract.TH

[abiFrom|contracts/build/SuperRareMarketAuctionV2.json|]
