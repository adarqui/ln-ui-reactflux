## Module Router

#### `routing`

``` purescript
routing :: Match Routes
```

#### `routeSignal`

``` purescript
routeSignal :: forall eff. Driver Input eff -> Routing eff Unit
```

#### `redirects`

``` purescript
redirects :: forall eff. Driver Input eff -> Maybe Routes -> Routes -> Routing eff Unit
```


