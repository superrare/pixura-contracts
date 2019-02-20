{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

module Pixura.Contracts.PixuraNFTContractGenerator where

import           Network.Ethereum.Contract.TH

[abiFrom|truffle/build/contracts/PixuraNFTContractGenerator.json|]
