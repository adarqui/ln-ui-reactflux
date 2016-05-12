module LN.View.Users.Workouts (
  renderView_Users_Workouts
) where



import Halogen              (ComponentHTML)
import Halogen.HTML.Indexed as H

import LN.Input.Types       (Input)
import LN.State.Types       (State)
import LN.View.Users        (usersLayout)



renderView_Users_Workouts :: String -> State -> ComponentHTML Input
renderView_Users_Workouts user_name st =
  usersLayout user_name st [
    H.div_ [
      H.h1_ [ H.text "Workouts." ]
    ]
  ]
