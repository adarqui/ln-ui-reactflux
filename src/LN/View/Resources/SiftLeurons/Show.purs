module LN.View.Resources.SiftLeurons.Show (
  renderView_Resources_SiftLeurons_Show
) where



import Halogen              (ComponentHTML)
import Halogen.HTML.Indexed as H

import LN.Input.Types       (Input)
import LN.State.Types       (State)



renderView_Resources_SiftLeurons_Show :: Int -> Int -> State -> ComponentHTML Input
renderView_Resources_SiftLeurons_Show resource_id offset _ = H.div_ [H.text "resources sift leurons show"]
