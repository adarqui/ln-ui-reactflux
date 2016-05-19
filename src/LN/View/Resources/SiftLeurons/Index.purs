module LN.View.Resources.SiftLeurons.Index (
  renderView_Resources_SiftLeurons_Index
) where



import Halogen              (ComponentHTML)
import Halogen.HTML.Indexed as H

import LN.Input.Types       (Input)
import LN.State.Types       (State)



renderView_Resources_SiftLeurons_Index :: Int -> State -> ComponentHTML Input
renderView_Resources_SiftLeurons_Index resource_id _ = H.div_ [H.text "resources sift leurons index"]
