module LN.View.Resources.Leurons.Index (
  renderView_Resources_Leurons_Index
) where



import Halogen              (ComponentHTML)
import Halogen.HTML.Indexed as H

import LN.Input.Types       (Input)
import LN.State.Types       (State)



renderView_Resources_Leurons_Index :: Int -> State -> ComponentHTML Input
renderView_Resources_Leurons_Index resource_id _ = H.div_ [H.text "resources leurons index"]
