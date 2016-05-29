## Module LN.State.Leuron

#### `LeuronRequestState`

``` purescript
type LeuronRequestState = { ty :: TyLeuron, exampleItem :: String, fact :: Fact, factList :: FactList, factList_listItem :: String, card :: Card, dcard :: DCard, dcardx :: DCardX, acronym :: Acronym, synonym :: Synonym, antonym :: Antonym, template :: Template, imageAssociation :: ImageAssociation, linearDemo :: LinearDemo, table :: Table, qa :: QA, ids :: Array Int }
```

#### `defaultLeuronRequestState`

``` purescript
defaultLeuronRequestState :: LeuronRequestState
```

#### `leuronRequestStateFromLeuronData`

``` purescript
leuronRequestStateFromLeuronData :: LeuronData -> LeuronRequestState -> LeuronRequestState
```


