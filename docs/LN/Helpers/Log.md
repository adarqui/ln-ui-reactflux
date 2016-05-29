## Module LN.Helpers.Log

#### `log`

``` purescript
log :: forall e. String -> Aff (console :: CONSOLE | e) Unit
```

made this in case we want to turn logging off in the future, ie, when we go into prod


