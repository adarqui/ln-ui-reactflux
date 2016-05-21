module LN.View.Resources.SiftLeuronsLinear.Index (
  renderView_Resources_SiftLeuronsLinear_Index
) where



import Halogen              (ComponentHTML)
import Halogen.HTML.Indexed as H

import LN.Input.Types       (Input)
import LN.Router.Types      (Routes(..), CRUD(..))
import LN.Router.Link       (linkTo)
import LN.State.Types       (State)



renderView_Resources_SiftLeuronsLinear_Index :: Int -> State -> ComponentHTML Input
renderView_Resources_SiftLeuronsLinear_Index resource_id _ =
  H.div_ [
    H.p_ [H.text "index"]
  ]
