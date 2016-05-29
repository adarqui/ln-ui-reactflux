## Module LN.Component.Types

#### `ComponentSlot`

``` purescript
type ComponentSlot s f g = Unit -> { component :: Component s f g, initialState :: s }
```

#### `EvalEff`

``` purescript
type EvalEff = forall eff. Eval Input State Input (LNEff eff) -> Eval Input State Input (LNEff eff)
```

#### `EvalEffP`

``` purescript
type EvalEffP = forall eff. Eval Input State Input (LNEff eff)
```

#### `LNEff`

``` purescript
type LNEff eff = Aff (LN eff)
```

#### `LN`

``` purescript
type LN eff = HalogenEffects (webStorage :: WebStorage, ajax :: AJAX, now :: Now, locale :: Locale, ws :: WEBSOCKET, console :: CONSOLE | eff)
```


