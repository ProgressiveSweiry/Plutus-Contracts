{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE NumericUnderscores #-}


module Trace where

import Control.Monad.Freer.Extras as Extras
import Data.Default               (Default (..))
import Data.Functor               (void)
import Ledger.TimeSlot
import Plutus.Trace
import Wallet.Emulator.Wallet
import Plutus.V1.Ledger.Value     as Value
import Ledger                     (toPubKeyHash)
import Data.Default               (Default (..))
import Ledger.Ada                 as Ada
import qualified Data.Map         as Map


import OrderValidator


test1 :: IO ()
test1 = runEmulatorTraceIO' def emCfg myTrace1

myTrace1 :: EmulatorTrace ()
myTrace1 = do
    h1 <- activateContractWallet (knownWallet 1) endpoints
    h2 <- activateContractWallet (knownWallet 2) endpoints
    let pkh = (\(Just x) -> x) $ toPubKeyHash $ mockWalletAddress $ knownWallet 1
    let order1 = Order { oCreator      = pkh
                        , oBuyCurrency = currency
                        , oBuyToken    = name
                        , oBuyAmount   = 5 }
    callEndpoint @"order" h1 $ OrderDatum
        { odOrder = order1
        }
    void $ waitNSlots 2
    callEndpoint @"redeem" h1 $ CancelOrder
    s <- waitNSlots 2
    Extras.logInfo $ "reached" ++ show s

test2 :: IO ()
test2 = runEmulatorTraceIO' def emCfg myTrace2

myTrace2 :: EmulatorTrace ()
myTrace2 = do
    h1 <- activateContractWallet (knownWallet 1) endpoints
    h2 <- activateContractWallet (knownWallet 2) endpoints
    let pkh = (\(Just x) -> x) $ toPubKeyHash $ mockWalletAddress $ knownWallet 1
    let order1 = Order { oCreator      = pkh
                        , oBuyCurrency = currency
                        , oBuyToken    = name
                        , oBuyAmount   = 5 }
    callEndpoint @"order" h1 $ OrderDatum
        { odOrder = order1
        }
    void $ waitNSlots 2
    s <- waitNSlots 2
    Extras.logInfo $ "reached" ++ show s

test3 :: IO ()
test3 = runEmulatorTraceIO' def emCfg myTrace3

myTrace3 :: EmulatorTrace ()
myTrace3 = do
    h1 <- activateContractWallet (knownWallet 1) endpoints
    h2 <- activateContractWallet (knownWallet 2) endpoints
    let pkh = (\(Just x) -> x) $ toPubKeyHash $ mockWalletAddress $ knownWallet 1
    let order1 = Order { oCreator      = pkh
                        , oBuyCurrency = currency
                        , oBuyToken    = name
                        , oBuyAmount   = 5 }
    callEndpoint @"order" h1 $ OrderDatum
        { odOrder = order1
        }
    void $ waitNSlots 2
    callEndpoint @"redeem" h2 $ FullMatch
    s <- waitNSlots 2
    Extras.logInfo $ "reached" ++ show s

test4 :: IO ()
test4 = runEmulatorTraceIO' def emCfg myTrace4

myTrace4 :: EmulatorTrace ()
myTrace4 = do
    h1 <- activateContractWallet (knownWallet 1) endpoints
    h2 <- activateContractWallet (knownWallet 2) endpoints
    let pkh = (\(Just x) -> x) $ toPubKeyHash $ mockWalletAddress $ knownWallet 1
    let order1 = Order { oCreator      = pkh
                        , oBuyCurrency = currency
                        , oBuyToken    = name
                        , oBuyAmount   = 20 }
    callEndpoint @"order" h1 $ OrderDatum
        { odOrder = order1
        }
    void $ waitNSlots 2
    callEndpoint @"redeem" h2 $ FullMatch
    void $ waitNSlots 2
    callEndpoint @"redeem" h1 $ CancelOrder
    s <- waitNSlots 2
    Extras.logInfo $ "reached" ++ show s


emCfg :: EmulatorConfig
emCfg = EmulatorConfig (Left $ Map.fromList [(knownWallet w, v) | w <- [1 .. 2]]) def def
  where
    v :: Value
    v = Ada.lovelaceValueOf 1_000_000_000 <> assetClassValue token 10


currency :: CurrencySymbol
currency = "3636"

name :: TokenName
name = "T"

token :: AssetClass
token = AssetClass (currency, name)


