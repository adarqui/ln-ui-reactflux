module LN.View.Portal.Resources (
  renderView_Portal_Resources
) where



import Halogen                         (ComponentHTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3       as B

import LN.Input.Types                  (Input)
import LN.State.Types                  (State)



renderView_Portal_Resources :: State -> ComponentHTML Input
renderView_Portal_Resources st = H.div_ []
