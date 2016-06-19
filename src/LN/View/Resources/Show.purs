module LN.View.Resources.Show (
  renderView_Resources_Show
) where



import Data.Maybe                      (Maybe(..))
import Halogen                         (ComponentHTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3       as B
import Optic.Core                      ((^.), (..))

import LN.Input.Types                  (Input)
import LN.Router.Link                  (linkToP_Classes, linkToP)
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.Router.Class.Params          (emptyParams)
import LN.State.Loading                (getLoading, l_currentResource)
import LN.State.Types                  (State)
import LN.View.Module.Loading          (renderLoading)
import LN.T                            ( ResourcePackResponse, _ResourcePackResponse, _ResourceResponse
                                       , resource_)



renderView_Resources_Show :: Int -> State -> ComponentHTML Input
renderView_Resources_Show resource_id st =

  case st.currentResource, getLoading l_currentResource st.loading of
       _, true          -> renderLoading
       Nothing, false   -> H.div_ [H.p_ [H.text "resource unavailable."]]
       Just pack, false -> renderView_Resources_Show' pack st



renderView_Resources_Show' :: ResourcePackResponse -> State -> ComponentHTML Input
renderView_Resources_Show' pack st =
  H.div [P.class_ B.containerFluid] [
    H.div [P.class_ B.pageHeader] [
      H.h1 [P.class_ B.textCenter] [H.text (resource.displayName)],
      H.p [P.class_ B.textCenter] [H.text (resource.description)]
    ],
    H.div [P.class_ B.container] [
      linkToP [] (Resources (EditI resource.id) emptyParams) "edit",
      linkToP [] (Resources (DeleteI resource.id) emptyParams) "delete"
    ],
    H.div [P.class_ B.container] [
      H.div [P.class_ B.listGroup] [
        linkToP_Classes [B.listGroupItem] [] (ResourcesLeurons resource.id Index emptyParams) "leurons",
        linkToP_Classes [B.listGroupItem] [] (ResourcesSiftLeurons resource.id emptyParams) "sift"
      ]
    ]
  ]

 where
 resource = pack ^. _ResourcePackResponse .. resource_ ^. _ResourceResponse
