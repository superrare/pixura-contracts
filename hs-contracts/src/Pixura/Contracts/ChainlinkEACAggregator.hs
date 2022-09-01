{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Pixura.Contracts.ChainlinkEACAggregator where

import Network.Ethereum.Contract.TH

[abiFrom|contracts/abis/ChainlinkEACAggregator.json|]