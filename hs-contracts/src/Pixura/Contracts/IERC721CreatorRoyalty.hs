{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

module Pixura.Contracts.IERC721CreatorRoyalty where

import           Network.Ethereum.Contract.TH

[abiFrom|contracts/build/IERC721CreatorRoyalty.json|]
