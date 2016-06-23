module LN.View.Home (
  renderView_Home
) where



import Halogen                (ComponentHTML)
import Halogen.HTML.Indexed   as H

import LN.Input.Types         (Input)
import LN.Router.Link         (linkToP)
import LN.Router.Class.CRUD   (CRUD(..))
import LN.Router.Class.Routes (Routes(..))
import LN.Router.Class.Params (emptyParams)



renderView_Home :: ComponentHTML Input
renderView_Home =
  H.div_ [
   H.h1_ [ H.text "Home" ],
   H.p_ [
     linkToP [] (Organizations Index emptyParams) "Organizations"
   ]
  ]
