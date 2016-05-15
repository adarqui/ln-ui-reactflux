module LN.View.Resources.Show (
  renderView_Resources_Show
) where



import Halogen              (ComponentHTML)
import Halogen.HTML.Indexed as H

import LN.Input.Types       (Input)
import LN.State.Types       (State)



renderView_Resources_Show :: String -> State -> ComponentHTML Input
renderView_Resources_Show resource_id _ = H.div_ [H.text "show"]
