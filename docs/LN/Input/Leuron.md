## Module LN.Input.Leuron

#### `InputLeuron`

``` purescript
data InputLeuron
  = InputLeuron_Nop1
  | InputLeuron_Set_Sift LeuronSift
  | InputLeuron_Mod Leuron_Mod
  | InputLeuron_Nop
```

#### `Leuron_Mod`

``` purescript
data Leuron_Mod
  = SetTitle String
  | EditTitle String
  | RemoveTitle
  | SetDescription String
  | EditDescription String
  | RemoveDescription
  | SetSection String
  | EditSection String
  | RemoveSection
  | SetPage Int
  | EditPage Int
  | RemovePage
  | SetExample String
  | AddExample String
  | EditExample Int String
  | DeleteExample Int
  | ClearExamples
  | AddStrength String
  | EditStrength Int String
  | DeleteStrength Int
  | ClearStrength
  | AddCategory (Array String)
  | EditCategory Int (Array String)
  | DeleteCategory Int
  | ClearCategories
  | AddTag String
  | EditTag Int String
  | DeleteTag Int
  | ClearTags
  | SetSpecificTo String
  | EditSpecificTo String
  | RemoveSpecificTo
  | SetData LeuronData
  | SetType TyLeuron
  | SetSt LeuronRequestState
  | Save Int
  | EditP Int
```


