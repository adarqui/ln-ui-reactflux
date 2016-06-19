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
import LN.Router.Class.Params          (emptyParams)



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

  , Organizations Index emptyParams
  , OrganizationsForums "adarq" Index emptyParams
  , OrganizationsForumsBoards "adarq" "forum" Index emptyParams
  , OrganizationsForumsBoardsThreads "adarq" "forum" "board" Index emptyParams
  , OrganizationsForumsBoardsThreadsPosts "adarq" "forum" "board" "thread" Index emptyParams

  , Users Index emptyParams
  , UsersProfile "adarq" emptyParams
  , UsersSettings "adarq" emptyParams
  , UsersPMs "adarq" emptyParams
  , UsersThreads "adarq" emptyParams
  , UsersThreadPosts "adarq" emptyParams
  , UsersWorkouts "adarq" emptyParams
  , UsersResources "adarq" emptyParams
  , UsersLeurons "adarq" emptyParams
  , UsersLikes "adarq" emptyParams

  , Resources Index emptyParams
  , ResourcesLeurons 1 Index emptyParams
  , ResourcesSiftLeurons 1 emptyParams
  , ResourcesSiftLeuronsLinear 1 Index emptyParams
  , ResourcesSiftLeuronsRandom 1 emptyParams
--  , Leurons Index emptyParams
  , Login
  , Logout
  ]
