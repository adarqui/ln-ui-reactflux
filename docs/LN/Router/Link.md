## Module LN.Router.Link

#### `(</>)`

``` purescript
infixl 9 apply' as </>
```

_left-associative / precedence 9_

#### `apply'`

``` purescript
apply' :: forall a b. (HasLink a, HasLink b) => (a -> b) -> a -> b
```

#### `updateUrl`

``` purescript
updateUrl :: forall e. Routes -> Aff (dom :: DOM | e) Unit
```

#### `linkTo`

``` purescript
linkTo :: Routes -> String -> HTML _ _
```

Create a link to a route, providing a string as the anchor name

#### `linkTo'`

``` purescript
linkTo' :: Routes -> Array (HTML _ _) -> HTML _ _
```

Create a link to a route, but provide an array of html elements instead of an anchor name

#### `linkToP`

``` purescript
linkToP :: Array Param -> Routes -> String -> HTML _ _
```

Create a link with Params

#### `linkToP_Classes`

``` purescript
linkToP_Classes :: Array ClassName -> Array Param -> Routes -> String -> HTML _ _
```

Create a link with class names as properties

#### `linkToP_Classes'`

``` purescript
linkToP_Classes' :: Array Param -> Routes -> String -> HTML _ _
```

#### `linkToP_Glyph`

``` purescript
linkToP_Glyph :: Array Param -> Routes -> ClassName -> HTML _ _
```

Create a link with Params, with a glyphicon

#### `linkToP_Glyph'`

``` purescript
linkToP_Glyph' :: Routes -> ClassName -> HTML _ _
```

Create a link with a glyphicon

#### `linkToHref`

``` purescript
linkToHref :: Routes -> IProp _ _
```

Create a link, but simply give us the property


