module LN.Input.Like (
  InputLike (..)
) where



import Data.Maybe (Maybe)

import LN.T       (ThreadPostPackResponse)


data InputLike
  = InputLike_Like    ThreadPostPackResponse
  | InputLike_Neutral ThreadPostPackResponse
  | InputLike_Dislike ThreadPostPackResponse
  | InputLike_Star    ThreadPostPackResponse
