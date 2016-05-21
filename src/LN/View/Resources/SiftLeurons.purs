module LN.View.Resources.SiftLeurons (
  renderView_Resources_SiftLeurons
) where



import Halogen              (ComponentHTML)
import Halogen.HTML.Indexed as H

import LN.Input.Types       (Input)
import LN.Router.Types      (Routes(..), CRUD(..))
import LN.Router.Link       (linkTo)
import LN.State.Types       (State)



renderView_Resources_SiftLeurons :: Int -> State -> ComponentHTML Input
renderView_Resources_SiftLeurons resource_id _ =
  H.div_ [
    H.ul_ [
      H.li_ [linkTo (ResourcesSiftLeuronsLinear resource_id Index []) "linear"],
      H.li_ [linkTo (ResourcesSiftLeuronsRandom resource_id []) "random"]
    ]
  ]
