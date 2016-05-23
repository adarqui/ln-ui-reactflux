module LN.View.Resources.SiftLeurons (
  renderView_Resources_SiftLeurons
) where



import Halogen                         (ComponentHTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3       as B

import LN.Input.Types                  (Input)
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.Router.Link                  (linkToP_Classes)
import LN.State.Types                  (State)



renderView_Resources_SiftLeurons :: Int -> State -> ComponentHTML Input
renderView_Resources_SiftLeurons resource_id _ =
  H.div [P.class_ B.listGroup] [
    linkToP_Classes [B.listGroupItem] [] (ResourcesSiftLeuronsLinear resource_id Index []) "linear",
    linkToP_Classes [B.listGroupItem] [] (ResourcesSiftLeuronsRandom resource_id []) "random"
  ]
