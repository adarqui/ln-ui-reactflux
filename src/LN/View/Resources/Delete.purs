module LN.View.Resources.Delete (
  renderView_Resources_Delete
) where



import Data.Maybe                      (Maybe(..))
import Halogen                         (ComponentHTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3       as B
import Optic.Core                      ((^.), (..))

import LN.Input.Types                  (Input)
import LN.Router.Link                  (linkToP_Classes, linkToP_Glyph')
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.State.Loading                (getLoading, l_currentResource)
import LN.State.Types                  (State)
import LN.View.Module.Loading          (renderLoading)
import LN.T                            ( ResourcePackResponse, _ResourcePackResponse, _ResourceResponse
                                       , resource_)



renderView_Resources_Delete :: Int -> State -> ComponentHTML Input
renderView_Resources_Delete resource_id st =

  case st.currentResource, getLoading l_currentResource st.loading of
       _, true          -> renderLoading
       Nothing, false   -> H.div_ [H.p_ [H.text "resource unavailable."]]
       Just pack, false -> renderView_Resources_Delete' pack st



renderView_Resources_Delete' :: ResourcePackResponse -> State -> ComponentHTML Input
renderView_Resources_Delete' pack st =
  H.div_ [H.p_ [H.text "Delete? <yes/no>"]]
 where
 resource = pack ^. _ResourcePackResponse .. resource_ ^. _ResourceResponse
