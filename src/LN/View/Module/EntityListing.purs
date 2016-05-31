module LN.View.Module.EntityListing (
  renderEntityListing,
  renderEntity
) where



import Data.Maybe                            (Maybe(..))
import Halogen                               (ComponentHTML)
import Halogen.HTML.Indexed                  as H
import Halogen.HTML.Properties.Indexed       as P
import Halogen.Themes.Bootstrap3             as B
import Prelude                               (map, ($))

import LN.Input.Types                        (Input)
import LN.Router.Link                        (linkTo', linkTo, linkToP)
import LN.Router.Types                       (Routes(..))
import LN.State.Entity                       (Entity)



renderEntityListing :: String -> Maybe Routes -> Array Entity -> ComponentHTML Input -> ComponentHTML Input
renderEntityListing title m_new entities page_numbers =
  H.div [P.class_ B.containerFluid] [
      H.div [P.class_ B.pageHeader] [
        H.h1 [P.class_ B.textCenter] [H.text title],
        case m_new of
             Nothing  -> H.div_ []
             Just new -> H.div_ [linkToP [] new "new"]
      ]
    , page_numbers
    , H.div [P.class_ B.container] [
        H.div [P.class_ B.row] $
          map renderEntity entities
      ]
    , page_numbers
  ]



renderEntity :: Entity -> ComponentHTML Input
renderEntity entity =
  H.div [P.class_ B.colXs4] [
    H.div_ [
        linkTo' entity.route
          [H.img [P.src entity.logo, P.alt entity.nick, P.classes [H.className "img-circle", H.className "img-thumbnail"]]]
--      , H.h3_ [H.text entity.displayNick]
      , H.h4_ [linkTo entity.route entity.nick]
    ]
  ]
