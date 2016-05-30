module LN.View.Module.FramePage (
  renderFramePage
) where



import Halogen                         (ComponentHTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Properties.Indexed as P
import Prelude                         (($))

import LN.Input.Types                  (Input)



renderFramePage :: ComponentHTML Input
renderFramePage =
  H.div_ [
    H.iframe [P.src "http://dev.stephendiehl.com/hask/#flags"]
  ]
