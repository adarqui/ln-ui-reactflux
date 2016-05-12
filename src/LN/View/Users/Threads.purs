module LN.View.Users.Threads (
  renderView_Users_Threads
) where



import Halogen              (ComponentHTML)
import Halogen.HTML.Indexed as H

import LN.Input.Types       (Input)
import LN.State.Types       (State)
import LN.View.Users        (usersLayout)



renderView_Users_Threads :: String -> State -> ComponentHTML Input
renderView_Users_Threads user_name st =
  usersLayout user_name st [
    H.div_ [
      H.h1_ [ H.text "Threads." ]
--      H.div_ (showUser st.me)
    ]
  ]
