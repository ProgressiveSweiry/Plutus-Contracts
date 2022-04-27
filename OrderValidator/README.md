# OrderValidator

This contract is made as a simple example of ordering tokens.

## Order

To place an order, wallet 1 sends datum (of type OrderDatum) to the contract, specifying what token and the amount to order. 

``` Haskell
data Order = Order
  { oCreator     :: !PubKeyHash
  , oBuyCurrency :: !CurrencySymbol
  , oBuyToken    :: !TokenName
  , oBuyAmount   :: !Integer
  }

data OrderDatum = OrderDatum
  { odOrder :: !Order
  }

```

The price of the tokens is 1 ADA per token as specified in the Order Endpoint 

```Haskell 
order :: AsContractError e => OrderDatum -> Contract w s e ()
order datum = do
    let od = odOrder datum
    let tx = Constraints.mustPayToTheScript datum ((amount $ oBuyAmount od) <> (lovelaceValueOf 3000000))
    void $ submitTxConstraints orderTypedValidator tx
  where
    amount x = Ada.lovelaceValueOf (x * 1000000)
```

## Redeem

To redeem an order, wallet 2 sends redeemer (FullMatch of type OrderAction)

```Haskell
data OrderAction = CancelOrder | FullMatch 
```

CancelOrder is used in case wallet 1 wants to cancel the order and get his ADA back from the script.

FullMatch is used with sending the amount of ADA needed for buyer and seller to receive their tokens and payment.


## Validator

Validator only validates on the following cases:

1. wallet 1 cancels the order, Validator will check if Redeemer was sent by the same wallet as the Datum.

2. wallet 2 redeem the order, Validator will check if wallet 1 and wallet 2 both got payed with tokens and ADA.

```Haskell

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
```

# Testing

EmulatorTrace is being used to test this smart contract.

To test this contract build it with cabal:

```
> cabal clean
> cabal update
> cabal build
```

Then let's try the Trace module:

```
> cabal repl

Prelude OrderValidator> :l Trace
```

Now we can use test1 - test5 to check different scenarios.

## Examples:

### test1

This test checks CancelOrder on wallet 1, funds should return to wallet 1 (minus fees)

``` 
Prelude Trace> test1

...

Final balances
Wallet 7ce812d7a4770bbf58004067665c3a48f28ddd58: 
    {3636, "T"}: 10
    {, ""}: 1000000000
Wallet 872cb83b5ee40eb23bfdab1772660c822a48d491: 
    {3636, "T"}: 10
    {, ""}: 999995105
```

We can see that wallet 1 canceled the order in the Trace.

### test3 

This test checks FullMatch on wallet 2, wallet 1 should receive the tokens and wallet 2 should receive the ADA.

```
Prelude Trace> test3

...
Final balances
Wallet 7ce812d7a4770bbf58004067665c3a48f28ddd58: 
    {3636, "T"}: 5
    {, ""}: 1004995115
Wallet 872cb83b5ee40eb23bfdab1772660c822a48d491: 
    {3636, "T"}: 15
    {, ""}: 994999990
```



