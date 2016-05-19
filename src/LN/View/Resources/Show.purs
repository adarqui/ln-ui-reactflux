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
import LN.Router.Internal              (linkToP)
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.State.Types                  (State)
import LN.T



renderView_Resources_Show :: String -> State -> ComponentHTML Input
renderView_Resources_Show resource_sid st =

  case st.currentResource of
       Nothing   -> H.div_ [H.text "Resource Unavailable"]
       Just pack -> renderView_Resources_Show' pack st



renderView_Resources_Show' :: ResourcePackResponse -> State -> ComponentHTML Input
renderView_Resources_Show' pack st =

  H.div [P.class_ B.containerFluid] [
    H.div [P.class_ B.pageHeader] [
      H.h1 [P.class_ B.textCenter] [H.text (resource.title)],
      H.p [P.class_ B.textCenter] [H.text (resource.description)]
    ],
    H.div [P.class_ B.container] [
      H.ul_ [
        H.li_ [linkToP [] (ResourcesLeurons resource.id Index []) "leurons"],
        H.li_ [linkToP [] (ResourcesSiftLeurons resource.id Index []) "sift"]
      ]
    ]
  ]

 where
 resource = pack ^. _ResourcePackResponse .. resource_ ^. _ResourceResponse
