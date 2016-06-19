module LN.Input.Like (
  InputLike (..)
) where



import Data.Maybe (Maybe)

import LN.T       (Ent, LikeResponse, StarResponse)


data InputLike
  = InputLike_Like    Ent Int (Maybe LikeResponse)
  | InputLike_Neutral Ent Int (Maybe LikeResponse)
  | InputLike_Dislike Ent Int (Maybe LikeResponse)
  | InputLike_Un      Ent Int (Maybe LikeResponse)
