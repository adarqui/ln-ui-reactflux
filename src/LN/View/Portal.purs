module LN.View.Portal (
  renderView_Portal
) where



import Halogen                         (ComponentHTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3       as B
import Prelude                         (show, map, ($))

import LN.Input.Types                  (Input)
import LN.Router.Link                  (linkToP_Classes)
import LN.Router.Types                 (Routes(..), CRUD(..))



renderView_Portal :: ComponentHTML Input
renderView_Portal =
  H.div_ [
    H.div [P.class_ B.pageHeader] [
      H.h2_ [ H.text "Portal" ]
    ],
    H.div [P.class_ B.listGroup] $ map renderPortalRow links
  ]



renderPortalRow :: Routes -> ComponentHTML Input
renderPortalRow route =
  linkToP_Classes [B.listGroupItem] [] route (show route)



links :: Array Routes
links =
  [ Home
  , About
  , Portal

  , Me

  , Errors

  , Organizations Index []
  , OrganizationsForums "adarq" Index []
  , OrganizationsForumsBoards "adarq" "forum" Index []
  , OrganizationsForumsBoardsThreads "adarq" "forum" "board" Index []
  , OrganizationsForumsBoardsThreadsPosts "adarq" "forum" "board" "thread" Index []

  , Users Index []
  , UsersProfile "adarq" []
  , UsersSettings "adarq" []
  , UsersPMs "adarq" []
  , UsersThreads "adarq" []
  , UsersThreadPosts "adarq" []
  , UsersWorkouts "adarq" []
  , UsersResources "adarq" []
  , UsersLeurons "adarq" []
  , UsersLikes "adarq" []

  , Resources Index []
  , ResourcesLeurons 1 Index []
  , ResourcesSiftLeurons 1 []
  , ResourcesSiftLeuronsLinear 1 Index []
  , ResourcesSiftLeuronsRandom 1 []
--  , Leurons Index []
  , Login
  , Logout
  ]
