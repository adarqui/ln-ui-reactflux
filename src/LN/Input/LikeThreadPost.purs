module LN.Input.LikeThreadPost (
  InputLikeThreadPost (..)
) where



data InputLikeThreadPost
  = InputLikeThreadPost_Like    Int
  | InputLikeThreadPost_Neutral Int
  | InputLikeThreadPost_Dislike Int
  | InputLikeThreadPost_Star    Int
