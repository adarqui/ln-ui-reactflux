module LN.View.Portal (
  renderView_Portal
) where



import Halogen              (ComponentHTML)
import Halogen.HTML.Indexed as H
import Prelude              (show, map, ($))

import LN.Input.Types       (Input)
import LN.Router.Internal   (linkToP)
import LN.Router.Types      (links)



renderView_Portal :: ComponentHTML Input
renderView_Portal =
  H.div_
    [ H.h1_ [ H.text "Portal" ]
    , H.ul_ $ map (\lnk -> H.li_ [linkToP [] lnk (show lnk)]) links
    ]
