module LN.View.Errors (
  renderView_Errors
) where



import Halogen              (ComponentHTML)
import Halogen.HTML.Indexed as H

import LN.Input.Types       (Input)
import LN.State.Types       (State)



renderView_Errors :: State -> ComponentHTML Input
renderView_Errors st =
  H.div_ [
    H.h1_ [ H.text "errors"]
 ]
