module LN.View.Resources.Index (
  renderView_Resources_Index
) where



import Halogen              (ComponentHTML)
import Halogen.HTML.Indexed as H

import LN.Input.Types       (Input(..))
import LN.State.Types       (State)
import LN.View.Portal.Resources



renderView_Resources_Index :: State -> ComponentHTML Input
renderView_Resources_Index _ = H.div_ [H.text "index"]
-- renderView_Resources_Index = renderView_Portal_Resources
