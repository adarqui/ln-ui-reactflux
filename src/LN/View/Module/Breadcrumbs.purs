module LN.View.Module.Breadcrumbs (
  renderBreadcrumbs
) where



import Data.Tuple                            (Tuple(..))
import Halogen                               (ComponentHTML)
import Halogen.HTML.Indexed                  as H
import Halogen.HTML.Properties.Indexed       as P
import Halogen.Themes.Bootstrap3             as B
import Prelude                               (map, ($))

import LN.Input.Types                        (Input)
import LN.Router.Link                        (linkTo)
import LN.Router.Types                       (Routes, crumb)
import LN.State.Types                        (State)



renderBreadcrumbs :: Routes -> State -> ComponentHTML Input
renderBreadcrumbs route st =
  H.div_ [
    H.ol [P.classes [B.breadcrumb]] $
      map (\(Tuple breadcrumb name) -> H.li [] [linkTo breadcrumb name]) (crumb route st)
  ]



{-
<ol class="breadcrumb">
  <li><a href="#">Home</a></li>
  <li><a href="#">Private</a></li>
  <li><a href="#">Pictures</a></li>
  <li class="active">Vacation</li> 
</ul>
-}
