{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Pixura.Contracts.EthOracle where

import Network.Ethereum.Contract.TH (abiFrom)

[abiFrom|contracts/no-gen-abis/EthOracle.json|]
