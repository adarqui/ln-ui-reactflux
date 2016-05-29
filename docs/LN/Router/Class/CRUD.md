## Module LN.Router.Class.CRUD

#### `CRUD`

``` purescript
data CRUD
  = Index
  | Show String
  | ShowI Int
  | ShowN Number
  | ShowB Boolean
  | New
  | Edit String
  | EditI Int
  | EditN Number
  | Delete String
  | DeleteI Int
  | DeleteN Number
```

##### Instances
``` purescript
Generic CRUD
Eq CRUD
HasLink CRUD
```


