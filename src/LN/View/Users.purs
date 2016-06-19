module LN.View.Users (
  renderView_Users,
  usersLayout,
  showUser
) where



import Data.Maybe                      (Maybe(..))
import Halogen                         (ComponentHTML, HTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3       as B
import Optic.Core                      ((^.), (..))
import Prelude                         (($), (<>))

import LN.View.Util                    (showIfSelf_UserNick)
import LN.Input.Types                  (Input)
import LN.State.Types                  (State)
import LN.Router.Link                  (linkToP)
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.Router.Class.Params          (emptyParams)
import LN.T



usersLayout :: String -> State -> Array (HTML _ _) -> HTML _ _
usersLayout user_nick st page =
  H.div [P.class_ B.containerFluid] [
    H.div [P.classes [B.colSm2]] [
      H.ul [P.class_ B.listUnstyled] [
        H.li_ [linkToP [] (Users (Show user_nick) emptyParams)        "Users"],
        H.li_ [linkToP [] (UsersProfile user_nick emptyParams)     "Profile"],
        showIfSelf_UserNick st user_nick [H.li_ [linkToP [] (UsersSettings user_nick emptyParams)    "Settings"]],
        showIfSelf_UserNick st user_nick [H.li_ [linkToP [] (UsersPMs user_nick emptyParams)         "Personal Messages"]],
        H.li_ [linkToP [] (UsersThreads user_nick emptyParams)     "Threads"],
        H.li_ [linkToP [] (UsersThreadPosts user_nick emptyParams) "Posts"],
        H.li_ [linkToP [] (UsersWorkouts user_nick emptyParams)    "Workouts"],
        H.li_ [linkToP [] (UsersResources user_nick emptyParams)   "Resources"],
        H.li_ [linkToP [] (UsersLeurons user_nick emptyParams)     "Leurons"],
        H.li_ [linkToP [] (UsersLikes user_nick emptyParams)       "Likes"]
      ]
    ],
    H.div [P.class_ B.colSm10] page
  ]
--    H.div [P.class_ B.colLg12] page



renderView_Users :: String -> State -> ComponentHTML Input
renderView_Users user_nick st =
  usersLayout user_nick st [
    H.div_ [
      H.h1_ [ H.text "me." ],
      H.div_ (showUser st.me)
    ]
  ]


showUser :: forall a b. Maybe UserPackResponse -> Array (HTML a b)
showUser Nothing = [ H.p_ [ H.text "login.." ] ]
showUser (Just (UserPackResponse user)) =
  [
      H.p_ [ H.text ("hello, " <> user ^. user_ ^. _UserResponse .. nick_) ]
    , H.p_ [ H.text $ user ^. user_ ^. _UserResponse .. displayNick_ ]
    , H.p_ [ H.text $ user ^. user_ ^. _UserResponse .. name_ ]
    , H.p_ [ H.text $ user ^. user_ ^. _UserResponse .. email_ ]
--    , H.p_ [ H.text $ user ^. user_ ^. _UserResponse .. plugin_ ]
--    , H.p_ [ H.text $ user ^. user_ ^. _UserResponse .. ident_ ]
{-
    , H.p_ [ H.text $ show user.isActive ]
    , H.p_ [ H.text $ show user.createdAt ]
    , H.p_ [ H.text $ show user.createdAt ]
    , H.p_ [ H.text $ show user.modifiedAt ]
    , H.p_ [ H.text $ show user.deactivatedAt ]
    -}
  ]
