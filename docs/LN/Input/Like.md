## Module LN.Input.Like

#### `InputLike`

``` purescript
data InputLike
  = InputLike_Like Ent Int (Maybe LikeResponse)
  | InputLike_Neutral Ent Int (Maybe LikeResponse)
  | InputLike_Dislike Ent Int (Maybe LikeResponse)
```


