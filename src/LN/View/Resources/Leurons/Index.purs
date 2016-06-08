module LN.View.Resources.Leurons.Index (
  renderView_Resources_Leurons_Index
) where



import Halogen                         (ComponentHTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3       as B

import LN.Input.Types                  (Input)
import LN.Router.Link                  (linkToP)
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.State.Types                  (State)



renderView_Resources_Leurons_Index :: Int -> State -> ComponentHTML Input
renderView_Resources_Leurons_Index resource_id _ =
  H.div_ [
    linkToP [] (ResourcesLeurons resource_id New []) "new",
    H.p_ [H.text "resources leurons index"]
  ]
