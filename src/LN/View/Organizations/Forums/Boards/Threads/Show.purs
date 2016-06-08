module LN.View.Organizations.Forums.Boards.Threads.Show (
  renderView_Organizations_Forums_Boards_Threads_Show
) where



import Daimyo.Data.ArrayList           (listToArray)
import Data.Map                        as M
import Data.Maybe                      (Maybe(..), maybe)
import Halogen                         (ComponentHTML, HTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.Events.Indexed     as E
import Halogen.Themes.Bootstrap3       as B
import Optic.Core                      ((^.), (..))
import Prelude                         (id, show, map, ($), (<>), (-))

import LN.Input.Types                  (Input (..))
import LN.Input.ThreadPost             (InputThreadPost (..))
import LN.Router.Link                  (linkTo, linkToP)
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.State.Types                  (State)
import LN.State.User                   (usersMapLookup, usersMapLookup_ToNick, usersMapLookup_ToUser)
import LN.View.ThreadPosts.Show        (renderView_ThreadPosts_Show)
import LN.View.Module.Gravatar         (renderGravatarForUser)
import LN.View.Module.Loading          (renderLoading)
import LN.View.Module.Like             (renderLike)
import LN.View.Module.PageNumbers      (renderPageNumbers)
import LN.T                            ( Ent(..)
                                       , UserSanitizedPackResponse, ThreadPackResponse, BoardPackResponse
                                       , ForumPackResponse, OrganizationPackResponse, PostData(PostDataBBCode)
                                       , Size(Medium), ThreadPostStatResponse(ThreadPostStatResponse)
                                       , _UserSanitizedStatResponse, stat_, _UserSanitizedPackResponse
                                       , signature_, _ProfileResponse, profile_, _ThreadPostStatResponse
                                       , _ThreadPostPackResponse, _ThreadPostResponse, threadPost_, body_
                                       , _ThreadPostRequest, _ThreadResponse, thread_, _ThreadPackResponse
                                       , _BoardResponse, board_, _BoardPackResponse, _ForumResponse, forum_
                                       , _ForumPackResponse, _OrganizationResponse, organization_
                                       , _OrganizationPackResponse)



renderView_Organizations_Forums_Boards_Threads_Show :: String -> State -> ComponentHTML Input
renderView_Organizations_Forums_Boards_Threads_Show thread_name st =

  case st.currentOrganization, st.currentForum, st.currentBoard, st.currentThread of

       Just org_pack, Just forum_pack, Just board_pack, Just thread_pack ->

         renderView_Organizations_Forums_Boards_Threads_Show' org_pack forum_pack board_pack thread_pack st

       _,             _,               _,              _                 -> renderLoading



renderView_Organizations_Forums_Boards_Threads_Show'
  :: OrganizationPackResponse
  -> ForumPackResponse
  -> BoardPackResponse
  -> ThreadPackResponse
  -> State
  -> ComponentHTML Input
renderView_Organizations_Forums_Boards_Threads_Show' org_pack forum_pack board_pack thread_pack st =
  H.div [P.class_ B.containerFluid] [
    H.div [P.class_ B.pageHeader] [
      H.h2_ [H.text thread.name],
        H.div_ [linkToP [] (OrganizationsForumsBoardsThreads org.name forum.name board.name (EditI 0) []) "sticky"],
        H.div_ [linkToP [] (OrganizationsForumsBoardsThreads org.name forum.name board.name (EditI 0) []) "edit"],
        H.div_ [linkToP [] (OrganizationsForumsBoardsThreads org.name forum.name board.name (DeleteI 0) []) "delete"]
    ],
    H.div [] [renderView_ThreadPosts_Show st]
  ]
  where
  org    = org_pack ^. _OrganizationPackResponse .. organization_ ^. _OrganizationResponse
  forum  = forum_pack ^. _ForumPackResponse .. forum_ ^. _ForumResponse
  board  = board_pack ^. _BoardPackResponse .. board_ ^. _BoardResponse
  thread = thread_pack ^. _ThreadPackResponse .. thread_ ^. _ThreadResponse
