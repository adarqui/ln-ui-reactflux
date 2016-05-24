module LN.View.Loading (
  renderView_Loading
) where



import Halogen                         (ComponentHTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Properties.Indexed as P
import Prelude                         (($))

import LN.Input.Types                  (Input)



renderView_Loading :: ComponentHTML Input
renderView_Loading =
  H.div_ [
    H.img [P.src $ "https://leuro.adarq.org/static/img/loading/2.gif"]
  ]
