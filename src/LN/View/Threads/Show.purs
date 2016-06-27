module LN.View.Threads.Show (
  renderView_Threads_Show,
  renderView_Threads_Show'
) where



import LN.ArrayList           (listToArray)
import Data.Map                        as M
import Data.Maybe                      (Maybe(..), maybe)
import Halogen                         (ComponentHTML, HTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3       as B
import Optic.Core                      ((^.), (..))
import Prelude                         (id, show, map, ($), (<>), (==), (||))

import LN.Access
import LN.Input.Types                  (Input)
import LN.Router.Link                  (linkToP, linkToP_Classes)
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.Router.Class.Params          (emptyParams)
import LN.State.Types                  (State)
import LN.State.User                   (usersMapLookup_ToUser)
-- import LN.View.Module.CreateThread     (renderCreateThread)
import LN.View.Helpers
import LN.View.ThreadPosts.Show        (renderView_ThreadPosts_Show')
import LN.View.Module.Gravatar         (renderGravatarForUser)
import LN.View.Module.Loading          (renderLoading)
import LN.View.Module.PageNumbers      (renderPageNumbers)
import LN.T                            ( BoardPackResponse, ForumPackResponse, OrganizationPackResponse, ThreadPackResponse
                                       , Size(Small), ThreadPostResponse(ThreadPostResponse)
                                       , UserSanitizedResponse(UserSanitizedResponse), latestThreadPostUser_
                                       , _ThreadPackResponse, latestThreadPost_, _ThreadStatResponse, stat_
                                       , _ThreadResponse, thread_, _BoardResponse, board_, _BoardPackResponse
                                       , _ForumResponse, forum_, _ForumPackResponse, _OrganizationResponse
                                       , organization_, _OrganizationPackResponse)



renderView_Threads_Show :: State -> ComponentHTML Input
renderView_Threads_Show st =

  case st.currentOrganization, st.currentForum, st.currentBoard, st.currentThread, st.currentThreadPostRequest, st.currentThreadPostRequestSt of

       Just org_pack, Just forum_pack, Just board_pack, Just thread_pack, Just post_req, Just post_req_st ->

         renderView_Threads_Show' st.meId org_pack forum_pack board_pack thread_pack $
           renderView_ThreadPosts_Show' st.meId org_pack forum_pack board_pack thread_pack st.threadPosts st.threadPostsPageInfo st.currentPage st.usersMap post_req post_req_st

       _, _, _, _, _, _                -> renderLoading



renderView_Threads_Show' :: Int -> OrganizationPackResponse -> ForumPackResponse -> BoardPackResponse -> ThreadPackResponse -> HTML _ _ -> ComponentHTML Input
renderView_Threads_Show' me_id org_pack forum_pack board_pack thread_pack plumbing_posts =

  H.div [P.class_ B.containerFluid] [
    H.div [P.class_ B.pageHeader] [
      H.h2_ [H.text thread.name],
      buttonGroup_HorizontalSm1 [
        -- ACCESS:
        -- * Update: edit thread settings
        -- * Delete: delete thread settings
        --
        permissionsHTML'
          thread_pack'.permissions
          permCreateEmpty
          permReadEmpty
          (\_ -> button_editThread $ OrganizationsForumsBoardsThreads org.name forum.name board.name (Edit thread.name) emptyParams)
          (\_ -> button_deleteThread $ OrganizationsForumsBoardsThreads org.name forum.name board.name (Delete thread.name) emptyParams)
          permExecuteEmpty
      ]
    ],
    H.div [] [plumbing_posts]
  ]

  where
  org          = org_pack ^. _OrganizationPackResponse .. organization_ ^. _OrganizationResponse
  forum        = forum_pack ^. _ForumPackResponse .. forum_ ^. _ForumResponse
  board        = board_pack ^. _BoardPackResponse .. board_ ^. _BoardResponse
  thread       = thread_pack ^. _ThreadPackResponse .. thread_ ^. _ThreadResponse
  thread_pack' = thread_pack ^. _ThreadPackResponse
