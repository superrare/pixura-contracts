{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

module Pixura.Contracts.CryptoPunks where

import           Network.Ethereum.Contract.TH

[abiFrom|../contracts/v4/build/CryptoPunksMarket.json|]
