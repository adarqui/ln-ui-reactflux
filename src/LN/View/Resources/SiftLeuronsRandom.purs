module LN.View.Resources.SiftLeuronsRandom (
  renderView_Resources_SiftLeuronsRandom
) where



import Halogen              (ComponentHTML)
import Halogen.HTML.Indexed as H

import LN.Input.Types       (Input)
import LN.Router.Types      (Routes(..))
import LN.Router.Link       (linkTo)
import LN.State.Types       (State)



renderView_Resources_SiftLeuronsRandom :: Int -> State -> ComponentHTML Input
renderView_Resources_SiftLeuronsRandom resource_id _ =
  H.div_ [
    H.p_ [H.text "random"]
  ]
