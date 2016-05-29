## Module LN.Eval.Profile

#### `eval_Profile`

``` purescript
eval_Profile :: EvalEff
```

#### `eval_Profile_Setter`

``` purescript
eval_Profile_Setter :: forall t72311 t72312 t72314 t72316 t72324 t72325 t72361. ((t72325 -> Identity t72324) -> { id :: Int, entityId :: Int, gender :: ProfileGender, birthdate :: Date, website :: Maybe String, location :: Maybe String, signature :: Maybe String, karmaGood :: Int, karmaBad :: Int, createdAt :: Maybe Date, modifiedAt :: Maybe Date } -> Identity { id :: Int, entityId :: Int, gender :: ProfileGender, birthdate :: Date, website :: Maybe String, location :: Maybe String, signature :: Maybe String, karmaGood :: Int, karmaBad :: Int, createdAt :: Maybe Date, modifiedAt :: Maybe Date }) -> t72324 -> t72361 -> Free (HalogenFP t72314 { me :: Maybe UserPackResponse | t72316 } t72312 t72311) t72361
```


