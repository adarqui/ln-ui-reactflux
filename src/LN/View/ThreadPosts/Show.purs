module LN.View.ThreadPosts.Show (
  renderView_ThreadPosts_Show,
  renderView_ThreadPosts_Show'
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
import LN.State.Types                  (State)
import LN.State.User                   (usersMapLookup, usersMapLookup_ToNick, usersMapLookup_ToUser)
import LN.View.ThreadPosts.Mod         (renderView_ThreadPosts_Mod, postDataToBody)
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



renderView_ThreadPosts_Show :: State -> ComponentHTML Input
renderView_ThreadPosts_Show st =

  case st.currentOrganization, st.currentForum, st.currentBoard, st.currentThread of

       Just org_pack, Just forum_pack, Just board_pack, Just thread_pack ->
         renderView_ThreadPosts_Show' org_pack forum_pack board_pack thread_pack st

       _,             _,               _,              _                 -> renderLoading



renderView_ThreadPosts_Show'
  :: OrganizationPackResponse -> ForumPackResponse -> BoardPackResponse -> ThreadPackResponse -> State -> ComponentHTML Input
renderView_ThreadPosts_Show' org_pack forum_pack board_pack thread_pack st =
  H.div_ [
      renderPageNumbers st.threadPostsPageInfo st.currentPage
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
                    H.p_ [linkTo (Users (Show (usersMapLookup_ToNick st post.userId)) []) (usersMapLookup_ToNick st post.userId)]
                  , renderGravatarForUser Medium (usersMapLookup_ToUser st post.userId)
                  , displayUserStats (usersMapLookup st post.userId)
                ]
              , H.div [P.class_ B.colXs8] [
                    linkTo (OrganizationsForumsBoardsThreadsPosts org.name forum.name board.name thread.name (ShowI post.id) []) (thread.name <> "/" <> show post.id)
                  , H.p_ [H.text $ show post.createdAt]
                  , H.p_ [H.text "quote / reply"]
                  , displayPostData post.body
                  , H.p_ [H.text $ maybe "" (\user -> maybe "" id $ user ^. _UserSanitizedPackResponse .. profile_ ^. _ProfileResponse .. signature_) (usersMapLookup st post.userId)]
                ]
              , H.div [P.class_ B.colXs1] [
                  H.div_ [linkToP [] (OrganizationsForumsBoardsThreadsPosts org.name forum.name board.name thread.name (EditI post.id) []) "edit"],
                  H.div_ [linkToP [] (OrganizationsForumsBoardsThreadsPosts org.name forum.name board.name thread.name (DeleteI post.id) []) "delete"]
                ]
              , H.div [P.class_ B.colXs1] [
                    renderLike Ent_ThreadPost post.id pack'.like pack'.star
                  , displayPostStats (ThreadPostStatResponse stats)
                ]
            ]

          ]
        ) $ listToArray $ M.values st.threadPosts)
        <>
        -- INPUT FORM AT THE BOTTOM
        [renderView_ThreadPosts_Mod Nothing st])
  , renderPageNumbers st.threadPostsPageInfo st.currentPage
  ]
  where
  body   = maybe "" (\p -> postDataToBody $ p ^. _ThreadPostRequest .. body_) st.currentThreadPostRequest
  org    = org_pack ^. _OrganizationPackResponse .. organization_ ^. _OrganizationResponse
  forum  = forum_pack ^. _ForumPackResponse .. forum_ ^. _ForumResponse
  board  = board_pack ^. _BoardPackResponse .. board_ ^. _BoardResponse
  thread = thread_pack ^. _ThreadPackResponse .. thread_ ^. _ThreadResponse




displayUserStats :: Maybe UserSanitizedPackResponse -> HTML _ _
displayUserStats Nothing = H.p_ [H.text "No stats."]
displayUserStats (Just user) =
  H.div_ [
    H.p_ [H.text $ "respect: " <> show stats.respect],
    H.p_ [H.text $ "threads: " <> show stats.threads],
    H.p_ [H.text $ "posts: " <> show stats.threadPosts],
    H.p_ [H.text $ "workouts: " <> show stats.workouts],
    H.p_ [H.text $ "resources: " <> show stats.resources],
    H.p_ [H.text $ "leurons: " <> show stats.leurons]
  ]
  where
  stats = user ^. _UserSanitizedPackResponse .. stat_ ^. _UserSanitizedStatResponse



displayPostStats :: ThreadPostStatResponse -> HTML _ _
displayPostStats (ThreadPostStatResponse stats) =
  H.div_ [
      H.p_ [H.text $ "score: " <> (show $ stats.likes - stats.dislikes)]
    , H.p_ [H.text $ "up: " <> show stats.likes]
    , H.p_ [H.text $ "neutral: " <> show stats.neutral]
    , H.p_ [H.text $ "down: " <> show stats.dislikes]
    , H.p_ [H.text $ "stars: " <> show stats.stars]
    , H.p_ [H.text $ "views: " <> show stats.views]
  ]



displayPostData :: PostData -> ComponentHTML Input
displayPostData (PostDataBBCode v) = H.pre_ [H.text v]
displayPostData _                  = H.p_ [H.text "unknown post body"]
