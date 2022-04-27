{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Deploy where

import           Codec.Serialise       (serialise)
import           Data.Aeson            (encode)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import           PlutusTx              (Data (..))
import qualified PlutusTx
import qualified Ledger

import OrderValidator


