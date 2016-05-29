## Module LN.State.PageInfo

#### `PageInfo`

``` purescript
type PageInfo = { currentPage :: Int, resultsPerPage :: Int, totalResults :: Int, totalPages :: Int, sortOrder :: SortOrderBy, order :: OrderBy }
```

#### `defaultPageInfo`

``` purescript
defaultPageInfo :: PageInfo
```

#### `defaultPageInfo_Users`

``` purescript
defaultPageInfo_Users :: PageInfo
```

#### `defaultPageInfo_Organizations`

``` purescript
defaultPageInfo_Organizations :: PageInfo
```

#### `defaultPageInfo_Forums`

``` purescript
defaultPageInfo_Forums :: PageInfo
```

#### `defaultPageInfo_Threads`

``` purescript
defaultPageInfo_Threads :: PageInfo
```

#### `defaultPageInfo_ThreadPosts`

``` purescript
defaultPageInfo_ThreadPosts :: PageInfo
```

#### `defaultPageInfo_Resources`

``` purescript
defaultPageInfo_Resources :: PageInfo
```

#### `defaultPageInfo_Leurons`

``` purescript
defaultPageInfo_Leurons :: PageInfo
```

#### `defaultPageInfo_Workouts`

``` purescript
defaultPageInfo_Workouts :: PageInfo
```

#### `RunPageInfo`

``` purescript
type RunPageInfo = { count :: Int, pageInfo :: PageInfo, params :: Array Param }
```

#### `runPageInfo`

``` purescript
runPageInfo :: CountResponses -> PageInfo -> RunPageInfo
```


