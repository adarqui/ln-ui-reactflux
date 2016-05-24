module LN.View.Resources.SiftLeuronsLinear.Index (
  renderView_Resources_SiftLeuronsLinear_Index
) where



import Halogen                         (ComponentHTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3       as B

import LN.Input.Types                  (Input)
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.Router.Link                  (linkToP_Classes)
import LN.State.Types                  (State)



renderView_Resources_SiftLeuronsLinear_Index :: Int -> State -> ComponentHTML Input
renderView_Resources_SiftLeuronsLinear_Index resource_id _ =
  H.div_ [
    H.div [P.class_ B.listGroup] [linkToP_Classes [B.listGroupItem] [] (ResourcesSiftLeuronsLinear resource_id (Show "0") []) "start"]
  ]
