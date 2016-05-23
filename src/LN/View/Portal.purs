module LN.View.Portal (
  renderView_Portal
) where



import Halogen                         (ComponentHTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3       as B
import Prelude                         (show, map, ($))

import LN.Input.Types                  (Input)
import LN.Router.Link                  (linkToP_Classes)
import LN.Router.Types                 (Routes, links)



renderView_Portal :: ComponentHTML Input
renderView_Portal =
  H.div_ [
    H.div [P.class_ B.pageHeader] [
      H.h2_ [ H.text "Portal" ]
    ],
    H.div [P.class_ B.listGroup] $ map renderPortalRow links
  ]



renderPortalRow :: Routes -> ComponentHTML Input
renderPortalRow route =
  linkToP_Classes [B.listGroupItem] [] route (show route)
