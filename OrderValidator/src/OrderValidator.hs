{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module OrderValidator where

import           Ledger.Typed.Scripts as Scripts
import           Ledger.Typed.Scripts.Validators as Validators
import           Plutus.V1.Ledger.Value 
import           Plutus.V1.Ledger.Address       as Address
import           Playground.Contract
import           Plutus.Contract
import           Ledger.Constraints as Constraints
import           Codec.Serialise         hiding ( encode )
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.ByteString.Short         as SBS
import           GHC.Generics                   ( Generic )
import           Ledger                  hiding ( singleton )
import           Ledger.Value                  as Value
import           Plutus.V1.Ledger.Ada           ( lovelaceValueOf )
import qualified Plutus.V1.Ledger.Api          as Plutus
import qualified PlutusTx
import           PlutusTx.Prelude        hiding ( Applicative(..)
                                                , unless
                                                )
import           PlutusPrelude
import           Data.Text                      (Text)
import           Data.Map                      as Map
import           Plutus.V1.Ledger.Ada         as Ada


data Order = Order
  { oCreator     :: !PubKeyHash
  , oBuyCurrency :: !CurrencySymbol
  , oBuyToken    :: !TokenName
  , oBuyAmount   :: !Integer
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

instance Eq Order where
  {-# INLINABLE (==) #-}
  a == b =
    (oCreator a == oCreator b)
      && (oBuyCurrency a == oBuyCurrency b)
      && (oBuyToken a == oBuyToken b)
      && (oBuyAmount a == oBuyAmount b)

PlutusTx.unstableMakeIsData ''Order
PlutusTx.makeLift ''Order

data OrderAction = CancelOrder | FullMatch deriving (Generic, FromJSON, ToJSON, ToSchema, ToArgument)


PlutusTx.unstableMakeIsData ''OrderAction
PlutusTx.makeLift ''OrderAction

data OrderDatum = OrderDatum
  { odOrder :: !Order
  } deriving            (Generic, FromJSON, ToJSON, ToSchema)


PlutusTx.unstableMakeIsData ''OrderDatum
PlutusTx.makeLift ''OrderDatum

data OrderScript
instance Scripts.ValidatorTypes OrderScript where
  type instance RedeemerType OrderScript = OrderAction
  type instance DatumType OrderScript = OrderDatum


{-# INLINABLE mkOrderValidator #-}
mkOrderValidator :: OrderDatum -> OrderAction -> ScriptContext -> Bool
mkOrderValidator od redeemer ctx = case redeemer of
  CancelOrder ->
    traceIfFalse "signature does not match creator in datum" checkSig
  FullMatch ->
    traceIfFalse "expected creator to get all of what she ordered" correctFull
      && traceIfFalse "only matches of pairs of orders allowed" twoParties

 where
  info :: TxInfo
  info = scriptContextTxInfo ctx

  order :: Order
  order = odOrder od

  orderedValue :: Order -> Value
  orderedValue o =
    Value.singleton (oBuyCurrency o) (oBuyToken o) (oBuyAmount o)

  ownInValue :: Value
  ownInValue = case (findOwnInput ctx) of
    Just inInfo -> (txOutValue . txInInfoResolved) inInfo

  resolvePubKeyHash :: Maybe PubKeyHash -> PubKeyHash
  resolvePubKeyHash pkh = case pkh of
    Just h -> h
    _      -> traceError "invalid public key hash"

  getsValue :: PubKeyHash -> Value
  getsValue h = sum
    [ txOutValue o'
    | o' <- txInfoOutputs info
    , resolvePubKeyHash (toPubKeyHash (txOutAddress o')) == h
    ]

  myScriptAddress :: Address
  myScriptAddress = case findOwnInput ctx of
    Just i -> txOutAddress $ txInInfoResolved i

  inputValueFromScript :: Value
  inputValueFromScript = sum
    [ txOutValue $ txInInfoResolved i
    | i <- txInfoInputs info
    ]

  getsAtLeastValue :: PubKeyHash -> Value -> Bool
  getsAtLeastValue h v = (getsValue h) `geq` v

  correctFull :: Bool
  correctFull =
    getsAtLeastValue (oCreator order)
                     ((orderedValue order) <> (lovelaceValueOf 1500000))
      &&    inputValueFromScript
      `geq` ((orderedValue order) <> ownInValue <> (lovelaceValueOf 1500000))

  checkSig :: Bool
  checkSig = txSignedBy info (oCreator order)

  twoParties :: Bool
  twoParties =
    let xs = [ i | i <- txInfoInputs info]
    in  case xs of
          [_, _] -> True
          _      -> False

-- | The address of the contract (the hash of its validator script).
contractAddress :: Ledger.Address
contractAddress = Ledger.scriptAddress orderValidator

-- compilation of the validator
orderTypedValidator :: Scripts.TypedValidator OrderScript
orderTypedValidator = Scripts.mkTypedValidator @OrderScript
    $$(PlutusTx.compile [|| mkOrderValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @OrderDatum @OrderAction

orderValidator :: Validator
orderValidator = Scripts.validatorScript orderTypedValidator

orderValidatorHash :: ValidatorHash
orderValidatorHash = Scripts.validatorHash orderTypedValidator

-- serialization
orderScript :: Script
orderScript = unValidatorScript orderValidator

-- Endpoints
order :: AsContractError e => OrderDatum -> Contract w s e ()
order datum = do
    let od = odOrder datum
    let tx = Constraints.mustPayToTheScript datum ((amount $ oBuyAmount od) <> (lovelaceValueOf 3000000))
    void $ submitTxConstraints orderTypedValidator tx
  where
    amount x = Ada.lovelaceValueOf (x * 1000000)


redeem :: AsContractError e => OrderAction -> Contract w s e ()
redeem myRedeemer = do
    unspentOutputs <- utxosAt contractAddress
    case myRedeemer of
        CancelOrder -> do
            let tx       = collectFromScript unspentOutputs myRedeemer
            void $ submitTxConstraintsSpending orderTypedValidator unspentOutputs tx
        FullMatch -> do
            let datum = _ciTxOutDatum $ head $ Map.elems unspentOutputs
            case datum of
                Right (Datum e) -> do
                    case PlutusTx.fromBuiltinData e of
                        Just d -> do
                            let tx       = mustPayToPubKey (PaymentPubKeyHash $ oCreator $ odOrder d) ((tokenVal $ odOrder d) <> (Ada.lovelaceValueOf 3000000))
                                        <> collectFromScript unspentOutputs myRedeemer
                            void $ submitTxConstraintsSpending orderTypedValidator unspentOutputs tx
                        Nothing -> do
                            return ()
                Left _ -> do 
                    return ()
  where
    tokenVal od = Value.singleton (oBuyCurrency od) (oBuyToken od) (oBuyAmount od)
    

endpoints :: Contract () OrderSchema Text ()
endpoints = awaitPromise (order' `select` redeem') >> endpoints
  where
    order'  = endpoint @"order" order
    redeem' = endpoint @"redeem" redeem


-- Schema Definitions

type OrderSchema =
    Endpoint "order" OrderDatum
    .\/ Endpoint "redeem" OrderAction

mkSchemaDefinitions ''OrderSchema

myToken :: KnownCurrency
myToken = KnownCurrency (ValidatorHash "f") "Token" (TokenName "T" :| [])

mkKnownCurrencies ['myToken]
