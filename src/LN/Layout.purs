module LN.Layout (
  defaultLayout
) where



import Data.Array                      ((:), length)
import Data.Maybe                      (Maybe(..))
import Halogen.HTML.Indexed            (HTML(), ClassName())
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3       as B
import Optic.Core                      ((^.), (..))
import Prelude                         (map, show, ($), (<>))

import LN.Debug                        (ifDebug)
import LN.Router.Link                  (linkToHref, linkTo)
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.Router.Class.Params          (emptyParams)
import LN.State.Types                  (State)
import LN.View.Module.Breadcrumbs      (renderBreadcrumbs)
import LN.T



row :: forall a b. Array (HTML a b) -> HTML a b
row = H.div [ P.class_ B.row ]



col :: forall a b. ClassName -> Array (HTML a b) -> HTML a b
col sz = H.div [ P.class_ sz ]



col' :: forall a b. Array ClassName -> Array (HTML a b) -> HTML a b
col' szs = H.div [ P.classes szs ]



defaultLayout :: State -> Array (HTML _ _) -> HTML _ _
defaultLayout st page =
  H.div [ P.class_ B.containerFluid ] [
    header st.me (length st.errors),

    ifDebug
      st
      (\_ -> H.p_ [H.text $ "DEBUG(currentPage): " <> show st.currentPage])
      (\_ -> H.div_ []),

    renderBreadcrumbs st.currentPage st,
    row page
  ]



-- container :: Array IProp -> HTML _ _
container attrs = H.div (P.class_ B.container : attrs)



container_ :: Array (HTML _ _) -> HTML _ _
container_ = container []



header :: Maybe UserPackResponse -> Int -> HTML _ _
header muser n_errors =
  H.div [P.class_ B.containerFluid] [
    H.nav [P.classes [B.navbarNav, B.navbarStaticTop]] [
      container_ [
        H.a [P.classes [B.navbarBrand], linkToHref Home] [H.text "Home"],
        H.ul [P.classes [B.navbarNav, B.nav, B.navTabs]] [
          H.li_ [linkTo About "About"],
          H.li_ [me],
          H.li_ [errors],
          H.li_ [linkTo Portal "Portal"]
        ],
--        case muser of
--          Nothing -> H.div [P.classes [B.nav, B.navbarNav, B.navTabs, B.navbarRight]] [linkTo Login "Log in"]
--          Just u  -> H.div [P.classes [B.nav, B.navbarNav, B.navTabs, B.navbarRight]] [linkTo Logout "Log out"]
--      ]
        case muser of
          Nothing ->
            H.ul [P.classes [B.nav, B.navbarNav, B.navTabs, B.navbarRight ]] [
              H.li_ [linkTo Login "Log in"]
            ]
          Just u ->
            H.ul [P.classes [B.nav, B.navbarNav, B.navTabs, B.navbarRight]] [
              H.li_ [linkTo Logout $ "Log out: " <> u ^. _UserPackResponse .. user_ ^. _UserResponse .. name_]
            ]
      ]
    ]
  ]
  where
  me = case muser of
            Nothing   -> linkTo NotFound "Me"
            Just user -> linkTo (Users (Show (user ^. _UserPackResponse .. user_ ^. _UserResponse .. nick_)) emptyParams) "Me"
  errors =
    -- TODO FIXME: use proper pill number
    linkTo Errors $ "Errors [" <> show n_errors <> "]"




footer :: forall a b. HTML a b
footer =
  H.footer [P.class_ (H.className "footer")] [
    H.text "LN",
    H.ul [] (map (\s -> H.li [] [H.text s]) ["About", "Contact"])
  ]
