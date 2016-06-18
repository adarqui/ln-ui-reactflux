module LN.View.Threads.Show (
  renderView_Threads_Show,
  renderView_Threads_Show'
) where



import Daimyo.Data.ArrayList           (listToArray)
import Data.Map                        as M
import Data.Maybe                      (Maybe(..), maybe)
import Halogen                         (ComponentHTML, HTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3       as B
import Optic.Core                      ((^.), (..))
import Prelude                         (id, show, map, ($), (<>), (==), (||))

import LN.Input.Types                  (Input)
import LN.Router.Link                  (linkToP, linkToP_Classes)
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.State.Types                  (State)
import LN.State.User                   (usersMapLookup_ToUser)
-- import LN.View.Module.CreateThread     (renderCreateThread)
import LN.View.Helpers
import LN.View.ThreadPosts.Index       (renderView_ThreadPosts_Index')
import LN.View.Module.Gravatar         (renderGravatarForUser)
import LN.View.Module.Loading          (renderLoading)
import LN.View.Module.OrderBy          (renderOrderBy)
import LN.View.Module.PageNumbers      (renderPageNumbers)
import LN.T                            ( BoardPackResponse, ForumPackResponse, OrganizationPackResponse, ThreadPackResponse
                                       , Size(Small), ThreadPostResponse(ThreadPostResponse)
                                       , UserSanitizedResponse(UserSanitizedResponse), latestThreadPostUser_
                                       , _ThreadPackResponse, latestThreadPost_, _ThreadStatResponse, stat_
                                       , _ThreadResponse, thread_, _BoardResponse, board_, _BoardPackResponse
                                       , _ForumResponse, forum_, _ForumPackResponse, _OrganizationResponse
                                       , organization_, _OrganizationPackResponse, isOwner_)



renderView_Threads_Show :: State -> ComponentHTML Input
renderView_Threads_Show st =

  case st.currentOrganization, st.currentForum, st.currentBoard, st.currentThread, st.currentThreadPostRequest, st.currentThreadPostRequestSt of

       Just org_pack, Just forum_pack, Just board_pack, Just thread_pack, Just post_req, Just post_req_st ->

         renderView_Threads_Show' st.meId org_pack forum_pack board_pack thread_pack $
           renderView_ThreadPosts_Index' org_pack forum_pack board_pack thread_pack st.threadPosts st.threadPostsPageInfo st.currentPage st.usersMap post_req post_req_st

       _, _, _, _, _, _                -> renderLoading



renderView_Threads_Show' :: Int -> OrganizationPackResponse -> ForumPackResponse -> BoardPackResponse -> ThreadPackResponse -> HTML _ _ -> ComponentHTML Input
renderView_Threads_Show' me_id org_pack forum_pack board_pack thread_pack plumbing_posts =

  H.div [P.class_ B.containerFluid] [
    H.div [P.class_ B.pageHeader] [
      H.h2_ [H.text thread.name],
      if org_owner || thread.userId == me_id
         then
           buttonGroup_HorizontalSm1 [
             glyphButtonLinkDef_Pencil $ OrganizationsForumsBoardsThreads org.name forum.name board.name (Edit thread.name) [],
             glyphButtonLinkDef_Trash $ OrganizationsForumsBoardsThreads org.name forum.name board.name (Delete thread.name) []
           ]
         else H.div_ []
    ],
    H.div [] [plumbing_posts]
  ]

  where
  org       = org_pack ^. _OrganizationPackResponse .. organization_ ^. _OrganizationResponse
  org_owner = org_pack ^. _OrganizationPackResponse .. isOwner_
  forum     = forum_pack ^. _ForumPackResponse .. forum_ ^. _ForumResponse
  board     = board_pack ^. _BoardPackResponse .. board_ ^. _BoardResponse
  thread    = thread_pack ^. _ThreadPackResponse .. thread_ ^. _ThreadResponse
