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
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.State.ThreadPost             (ThreadPostRequestState)
import LN.State.PageInfo               (PageInfo)
import LN.State.Types                  (State)
import LN.State.User                   (usersMapLookup', usersMapLookup_ToNick', usersMapLookup_ToUser')
import LN.View.Helpers
import LN.View.ThreadPosts.Mod         (renderView_ThreadPosts_Mod', postDataToBody)
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
         renderView_ThreadPosts_Index' org_pack forum_pack board_pack thread_pack st.threadPosts st.threadPostsPageInfo st.currentPage st.usersMap post_req post_req_st

       _, _, _, _, _ , _                 -> renderLoading



renderView_ThreadPosts_Index'
  :: OrganizationPackResponse
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
renderView_ThreadPosts_Index' org_pack forum_pack board_pack thread_pack post_packs posts_page_info posts_route users_map post_req post_req_st =
  H.div_ [
      renderPageNumbers posts_page_info posts_route
    , H.ul [P.class_ B.listUnstyled] (
        (map (\pack ->
          let
            pack' = pack ^. _ThreadPostPackResponse
            post  = pack ^. _ThreadPostPackResponse .. threadPost_ ^. _ThreadPostResponse
            stats = pack ^. _ThreadPostPackResponse .. stat_ ^. _ThreadPostStatResponse
          in
          H.li_ [

            H.div [P.class_ B.row] [
                H.div [P.class_ B.colXs2] [
                    H.p_ [linkTo (Users (Show (usersMapLookup_ToNick' users_map post.userId)) []) (usersMapLookup_ToNick' users_map post.userId)]
                  , renderGravatarForUser Medium (usersMapLookup_ToUser' users_map post.userId)
                  , displayUserStats (usersMapLookup' users_map post.userId)
                ]
              , H.div [P.class_ B.colXs8] [
                    linkTo (OrganizationsForumsBoardsThreadsPosts org.name forum.name board.name thread.name (ShowI post.id) []) (thread.name <> "/" <> show post.id)
                  , H.p_ [H.text $ show post.createdAt]
                  , H.p_ [H.text "quote / reply"]
                  , displayPostData post.body
                  , H.p_ [H.text $ maybe "" (\user -> maybe "" id $ user ^. _UserSanitizedPackResponse .. profile_ ^. _ProfileResponse .. signature_) (usersMapLookup' users_map post.userId)]
                ]
              , H.div [P.class_ B.colXs1] [
                    renderLike Ent_ThreadPost post.id pack'.like pack'.star
                  , displayPostStats (ThreadPostStatResponse stats)
                ]
              , H.div [P.class_ B.colXs1] [
                  buttonGroup_VerticalSm1 [
                    glyphButtonLinkDef_Pencil $ OrganizationsForumsBoardsThreadsPosts org.name forum.name board.name thread.name (EditI post.id) [],
                    glyphButtonLinkDef_Trash $ OrganizationsForumsBoardsThreadsPosts org.name forum.name board.name thread.name (DeleteI post.id) []
                  ]
                ]
            ]

          ]
        ) $ listToArray $ M.values post_packs)
        <>
        -- INPUT FORM AT THE BOTTOM
        [renderView_ThreadPosts_Mod' thread_pack Nothing post_req post_req_st])
  , renderPageNumbers posts_page_info posts_route
  ]
  where
  body   = postDataToBody $ post_req ^. _ThreadPostRequest .. body_
  org    = org_pack ^. _OrganizationPackResponse .. organization_ ^. _OrganizationResponse
  forum  = forum_pack ^. _ForumPackResponse .. forum_ ^. _ForumResponse
  board  = board_pack ^. _BoardPackResponse .. board_ ^. _BoardResponse
  thread = thread_pack ^. _ThreadPackResponse .. thread_ ^. _ThreadResponse




displayUserStats :: Maybe UserSanitizedPackResponse -> HTML _ _
displayUserStats Nothing = H.p_ [H.text "No stats."]
displayUserStats (Just user) =
  H.div_ [
    showBadge' "respect "   stats.respect,
    showBadge' "threads "   stats.threads,
    showBadge' "posts "     stats.threadPosts,
    showBadge' "workouts "  stats.workouts,
    showBadge' "resources " stats.resources,
    showBadge' "leurons "   stats.leurons
  ]
  where
  stats = user ^. _UserSanitizedPackResponse .. stat_ ^. _UserSanitizedStatResponse



displayPostStats :: ThreadPostStatResponse -> HTML _ _
displayPostStats (ThreadPostStatResponse stats) =
  H.div_ [
    showBadge' "score: "   $ stats.likes - stats.dislikes,
    showBadge' "up: "      stats.likes,
    showBadge' "neutral: " stats.neutral,
    showBadge' "down: "    stats.dislikes,
    showBadge' "stars: "   stats.stars,
    showBadge' "views: "   stats.views
  ]



displayPostData :: PostData -> ComponentHTML Input
displayPostData (PostDataBBCode v) = H.pre_ [H.text v]
displayPostData _                  = H.p_ [H.text "unknown post body"]
