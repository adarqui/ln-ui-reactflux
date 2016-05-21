module LN.View.Resources.SiftLeuronsLinear.Show (
  renderView_Resources_SiftLeuronsLinear_Show
) where



import Halogen              (ComponentHTML)
import Halogen.HTML.Indexed as H

import LN.Input.Types       (Input)
import LN.Router.Types      (Routes(..), CRUD(..))
import LN.Router.Link       (linkTo)
import LN.State.Types       (State)



renderView_Resources_SiftLeuronsLinear_Show :: Int -> String -> State -> ComponentHTML Input
renderView_Resources_SiftLeuronsLinear_Show resource_id s_offset _ =
  H.div_ [
    H.p_ [H.text s_offset]
  ]
