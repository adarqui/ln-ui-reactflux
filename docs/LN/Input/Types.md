## Module LN.Input.Types

#### `Input`

``` purescript
data Input a
  = Goto Routes a
  | AddError String String a
  | AddErrorF String ForeignError a
  | AddErrorApi String ApiError a
  | DelError Int a
  | ClearErrors a
  | GetUser String a
  | GetMe a
  | GetUsers a
  | GetUsers_MergeMap_ByUser (Array UserSanitizedResponse) a
  | GetUsers_MergeMap_ByUserId (Array Int) a
  | GetOrganizations a
  | GetOrganization String a
  | GetOrganizationForum String String a
  | GetOrganizationForumBoard String String String a
  | GetOrganizationForumBoardThread String String String String a
  | GetTeams a
  | GetForums a
  | GetForumsForOrg String a
  | GetBoards a
  | GetBoardsForForum Int a
  | GetThreads a
  | GetThreadsForBoard Int a
  | GetThreadPosts a
  | GetThreadPostsForThread Int a
  | GetThreadPost String a
  | GetThreadPostLikes a
  | GetPMs a
  | GetResources a
  | GetResourceId Int a
  | GetResourcesLeurons Int a
  | GetResourceLeuronLinear Int Int a
  | GetResourceLeuronRandom Int a
  | GetResourcesSiftLeurons Int a
  | GetLeurons a
  | GetLeuronId Int a
  | GetLeuronRandom a
  | ConnectSocket a
  | CompThreadPost InputThreadPost a
  | CompCreateThread InputCreateThread a
  | CompOrderBy InputOrderBy a
  | CompProfile InputProfile a
  | CompLike InputLike a
  | CompStar InputStar a
  | CompResource InputResource a
  | CompLeuron InputLeuron a
  | Nop a
```

#### `cResource`

``` purescript
cResource :: forall a. InputResource -> a -> Input a
```

Helpers for "components" and "subcomponents"

#### `cResourceMod`

``` purescript
cResourceMod :: forall a. Resource_Mod -> a -> Input a
```

#### `cLeuron`

``` purescript
cLeuron :: forall a. InputLeuron -> a -> Input a
```

#### `cLeuronMod`

``` purescript
cLeuronMod :: forall a. Leuron_Mod -> a -> Input a
```

#### `cLeuronNop`

``` purescript
cLeuronNop :: forall a. a -> Input a
```


