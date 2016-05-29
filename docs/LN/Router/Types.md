## Module LN.Router.Types

#### `Routing`

``` purescript
type Routing e = Aff (dom :: DOM, avar :: AVAR, err :: EXCEPTION | e)
```


### Re-exported from LN.Router.Class.CRUD:

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

### Re-exported from LN.Router.Class.Link:

#### `HasLink`

``` purescript
class HasLink a where
  link :: a -> Tuple String (Map String String)
```

### Re-exported from LN.Router.Class.OrderBy:

#### `HasOrderBy`

``` purescript
class HasOrderBy a where
  orderBy :: a -> Array OrderBy
```

### Re-exported from LN.Router.Class.Routes:

#### `Routes`

``` purescript
data Routes
  = Home
  | About
  | Me
  | Errors
  | Portal
  | Organizations CRUD Params
  | OrganizationsForums String CRUD Params
  | OrganizationsForumsBoards String String CRUD Params
  | OrganizationsForumsBoardsThreads String String String CRUD Params
  | OrganizationsForumsBoardsThreadsPosts String String String String CRUD
  | Users CRUD Params
  | UsersProfile String Params
  | UsersSettings String Params
  | UsersPMs String Params
  | UsersThreads String Params
  | UsersThreadPosts String Params
  | UsersWorkouts String Params
  | UsersResources String Params
  | UsersLeurons String Params
  | UsersLikes String Params
  | Resources CRUD Params
  | ResourcesLeurons Int CRUD Params
  | ResourcesSiftLeurons Int Params
  | ResourcesSiftLeuronsLinear Int CRUD Params
  | ResourcesSiftLeuronsRandom Int Params
  | Login
  | Logout
  | NotFound
```

##### Instances
``` purescript
Generic Routes
Eq Routes
HasLink Routes
HasCrumb Routes
HasOrderBy Routes
Show Routes
```

#### `HasCrumb`

``` purescript
class HasCrumb a where
  crumb :: a -> InternalState Routes -> Array (Tuple Routes String)
```

##### Instances
``` purescript
HasCrumb Routes
```

