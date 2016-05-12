module LN.View.Users.Show (
  renderView_Users_Show
) where



import Halogen              (ComponentHTML)
import Halogen.HTML.Indexed as H

import LN.Input.Types       (Input)
import LN.State.Types       (State)
import LN.View.Users        (usersLayout)



renderView_Users_Show :: String -> State -> ComponentHTML Input
renderView_Users_Show user_name st =
  usersLayout user_name st [
    H.div_ [
    ]
  ]
