module LN.Layout (
  defaultLayout
) where



import Data.Array                      hiding ((..))
import Data.Maybe                      (Maybe(..))
import Halogen.HTML.Indexed            (HTML(), ClassName())
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3       as B
import Optic.Core                      ((^.), (..))
import Prelude                         (map)

import LN.Router.Internal              (linkToHref, linkTo)
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.State.Types                  (State)
import LN.View.Module.Breadcrumbs      (renderBreadcrumbs)
import LN.State.Lens
import LN.T



row :: forall a b. Array (HTML a b) -> HTML a b
row = H.div [ P.class_ B.row ]

col :: forall a b. ClassName -> Array (HTML a b) -> HTML a b
col sz = H.div [ P.class_ sz ]

col' :: forall a b. Array ClassName -> Array (HTML a b) -> HTML a b
col' szs = H.div [ P.classes szs ]

defaultLayout :: State -> Array (HTML _ _) -> HTML _ _
defaultLayout st page =
--  H.div [ P.class_ B.container ]
  H.div [ P.class_ B.containerFluid ]
    [ header st.me
    , renderBreadcrumbs (st ^. stCurrentPage)
--    , row [ col' [B.colLg12] [renderBreadcrumbs (st ^. stCurrentPage)]]
    , row
        [ col' [B.colLg12] page ]
--        [ col' [ B.colLg10, B.colLgOffset1 ] page ]
    ]

-- container :: Array IProp -> HTML _ _
container attrs = H.div (P.class_ B.container : attrs)

container_ :: Array (HTML _ _) -> HTML _ _
container_ = container []



header :: Maybe UserPackResponse -> HTML _ _
header muser =
  H.div [ P.class_ B.containerFluid ] [
  H.nav [ P.classes [ B.navbarNav, B.navbarStaticTop] ]
    [ container_
      [ H.a [ P.classes [ B.navbarBrand ], linkToHref Home ]-- P.href (link Home) ]
        [ H.text "Home" ]
      , H.ul [ P.classes [ B.navbarNav, B.nav, B.navTabs] ]
        [
          H.li_ [ linkTo About "About" ]
        , H.li_ [me] -- [ linkTo Me "Me" ]
        , H.li_ [ linkTo Portal "Portal" ]
        ]
      , case muser of
             Nothing ->
                 H.ul [ P.classes [ B.nav, B.navbarNav, B.navTabs, B.navbarRight ] ]
                     [
                        H.li_ [ linkTo Login "Log in" ]
--                     , H.li_ [ linkTo Registration "Sign up" ]
                     ]
             Just u ->
                 H.ul [ P.classes [ B.nav, B.navbarNav, B.navTabs, B.navbarRight ] ]
                     [ H.li_ [ linkTo Logout "Log out" ] ]

      ]
    ]
  ]
  where
  me = case muser of
            Nothing -> linkTo NotFound "Me"
            Just user -> linkTo (Users (Show (user ^. _UserPackResponse .. user_ ^. _UserResponse .. nick_))) "Me"



footer :: forall a b. HTML a b
footer =
  H.footer [ P.class_ (H.className "footer") ]
    [ H.text "LN"
    , H.ul []
      (map (\s -> H.li [] [ H.text s ]) [ "About", "Contact" ] )
    ]
