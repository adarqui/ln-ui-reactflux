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
import LN.Router.Link                  (linkToP_Classes)
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.State.Types                  (State)
import LN.View.Loading                 (renderView_Loading)
import LN.T                            ( ResourcePackResponse, _ResourcePackResponse, _ResourceResponse
                                       , resource_)



renderView_Resources_Show :: String -> State -> ComponentHTML Input
renderView_Resources_Show resource_sid st =

  case st.currentResource of
       Nothing   -> renderView_Loading
       Just pack -> renderView_Resources_Show' pack st



renderView_Resources_Show' :: ResourcePackResponse -> State -> ComponentHTML Input
renderView_Resources_Show' pack st =

  H.div [P.class_ B.containerFluid] [
    H.div [P.class_ B.pageHeader] [
      H.h1 [P.class_ B.textCenter] [H.text (resource.title)],
      H.p [P.class_ B.textCenter] [H.text (resource.description)]
    ],
    H.div [P.class_ B.container] [
      H.div [P.class_ B.listGroup] [
        linkToP_Classes [B.listGroupItem] [] (ResourcesLeurons resource.id Index []) "leurons",
        linkToP_Classes [B.listGroupItem] [] (ResourcesSiftLeurons resource.id []) "sift"
      ]
    ]
  ]

 where
 resource = pack ^. _ResourcePackResponse .. resource_ ^. _ResourceResponse
