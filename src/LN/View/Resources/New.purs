module LN.View.Resources.New (
  renderView_Resources_New
) where



import Halogen              (ComponentHTML)
import Halogen.HTML.Indexed as H

import LN.Input.Types       (Input(..))
import LN.State.Types       (State)



renderView_Resources_New :: State -> ComponentHTML Input
renderView_Resources_New _ = H.div_ [H.text "new"]
