module LN.View.Users.PMs (
  renderView_Users_PMs
) where



import Halogen              (ComponentHTML)
import Halogen.HTML.Indexed as H

import LN.Input.Types       (Input)
import LN.State.Types       (State)
import LN.View.Users        (usersLayout)



renderView_Users_PMs :: String -> State -> ComponentHTML Input
renderView_Users_PMs user_name st =
  usersLayout user_name st [
    H.div_ [
      H.h1_ [ H.text "PMs." ]
    ]
  ]
