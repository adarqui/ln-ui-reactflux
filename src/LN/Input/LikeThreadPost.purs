module LN.Input.LikeThreadPost (
  InputLikeThreadPost (..)
) where



import Data.Maybe (Maybe)

import LN.T       (ThreadPostLikeResponse)


data InputLikeThreadPost
  = InputLikeThreadPost_Like    Int (Maybe ThreadPostLikeResponse)
  | InputLikeThreadPost_Neutral Int (Maybe ThreadPostLikeResponse)
  | InputLikeThreadPost_Dislike Int (Maybe ThreadPostLikeResponse)
  | InputLikeThreadPost_Star    Int
