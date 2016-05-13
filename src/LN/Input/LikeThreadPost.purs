module LN.Input.LikeThreadPost (
  InputLikeThreadPost (..)
) where



import Data.Maybe (Maybe)

import LN.T       (ThreadPostPackResponse)


data InputLikeThreadPost
  = InputLikeThreadPost_Like    ThreadPostPackResponse
  | InputLikeThreadPost_Neutral ThreadPostPackResponse
  | InputLikeThreadPost_Dislike ThreadPostPackResponse
  | InputLikeThreadPost_Star    ThreadPostPackResponse
