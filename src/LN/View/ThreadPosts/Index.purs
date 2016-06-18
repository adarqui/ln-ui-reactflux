module LN.View.ThreadPosts.Index (
  renderView_ThreadPosts_Index,
  renderView_ThreadPosts_Index'
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
import Prelude                         (id, show, map, ($), (<>), (-), (<<<))

import LN.Input.Types                  (Input(..), cThreadPostMod)
import LN.Input.ThreadPost             (InputThreadPost(..), ThreadPost_Mod(..))
import LN.Router.Link                  (linkTo, linkToP)
import LN.Router.Class.CRUD            (CRUD(..), TyCRUD(..))
import LN.Router.Class.Routes          (Routes(..))
import LN.State.ThreadPost             (ThreadPostRequestState)
import LN.State.PageInfo               (PageInfo)
import LN.State.Types                  (State)
import LN.State.User                   (usersMapLookup', usersMapLookup_ToNick', usersMapLookup_ToUser')
import LN.View.Helpers
import LN.View.ThreadPosts.Mod         (renderView_ThreadPosts_Mod')
import LN.View.ThreadPosts.Show        (renderView_ThreadPosts_Show_Single')
import LN.View.Module.Gravatar         (renderGravatarForUser)
import LN.View.Module.Loading          (renderLoading)
import LN.View.Module.Like             (renderLike)
import LN.View.Module.PageNumbers      (renderPageNumbers)
import LN.T                            ( Ent(..)
                                       , UserSanitizedPackResponse, ThreadPackResponse, BoardPackResponse
                                       , ForumPackResponse, OrganizationPackResponse, PostData(PostDataBBCode)
                                       , ThreadPostPackResponse
                                       , ThreadPostRequest
                                       , Size(Medium), ThreadPostStatResponse(ThreadPostStatResponse)
                                       , _UserSanitizedStatResponse, stat_, _UserSanitizedPackResponse
                                       , signature_, _ProfileResponse, profile_, _ThreadPostStatResponse
                                       , _ThreadPostPackResponse, _ThreadPostResponse, threadPost_, body_
                                       , _ThreadPostRequest, _ThreadResponse, thread_, _ThreadPackResponse
                                       , _BoardResponse, board_, _BoardPackResponse, _ForumResponse, forum_
                                       , _ForumPackResponse, _OrganizationResponse, organization_
                                       , _OrganizationPackResponse)



renderView_ThreadPosts_Index :: State -> ComponentHTML Input
renderView_ThreadPosts_Index st =

  case st.currentOrganization, st.currentForum, st.currentBoard, st.currentThread, st.currentThreadPostRequest, st.currentThreadPostRequestSt of

       Just org_pack, Just forum_pack, Just board_pack, Just thread_pack, Just post_req, Just post_req_st ->
         renderView_ThreadPosts_Index' st.meId org_pack forum_pack board_pack thread_pack st.threadPosts st.threadPostsPageInfo st.currentPage st.usersMap post_req post_req_st

       _, _, _, _, _ , _                 -> renderLoading



renderView_ThreadPosts_Index'
  :: Int
  -> OrganizationPackResponse
  -> ForumPackResponse
  -> BoardPackResponse
  -> ThreadPackResponse
  -> M.Map Int ThreadPostPackResponse
  -> PageInfo
  -> Routes
  -> M.Map Int UserSanitizedPackResponse
  -> ThreadPostRequest
  -> ThreadPostRequestState
  -> ComponentHTML Input
renderView_ThreadPosts_Index' me_id org_pack forum_pack board_pack thread_pack post_packs posts_page_info posts_route users_map post_req post_req_st =
  H.div_ [
      renderPageNumbers posts_page_info posts_route
    , H.ul [P.class_ B.listUnstyled] (
        (map (\post_pack ->
          H.li_ [renderView_ThreadPosts_Show_Single' me_id org_pack forum_pack board_pack thread_pack post_pack users_map]
        ) $ listToArray $ M.values post_packs)
        <>
        -- INPUT FORM AT THE BOTTOM
        [renderView_ThreadPosts_Mod' TyCreate thread_pack Nothing post_req post_req_st])
  , renderPageNumbers posts_page_info posts_route
  ]
  where
  org    = org_pack ^. _OrganizationPackResponse .. organization_ ^. _OrganizationResponse
  forum  = forum_pack ^. _ForumPackResponse .. forum_ ^. _ForumResponse
  board  = board_pack ^. _BoardPackResponse .. board_ ^. _BoardResponse
  thread = thread_pack ^. _ThreadPackResponse .. thread_ ^. _ThreadResponse
