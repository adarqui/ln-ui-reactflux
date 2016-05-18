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
import LN.Router.Internal              (linkTo)
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.State.Types                  (State)
import LN.State.User                   (usersMapLookup, usersMapLookup_ToNick, usersMapLookup_ToUser)
import LN.View.Module.Gravatar         (renderGravatarForUser)
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

       _,             _,               _,              _                 -> H.div_ [H.text "Unavailable"]



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
        H.h2_ [H.text thread.name]
    ],
    H.div [] [renderPosts org.name forum.name board.name thread.name st]
  ]
  where
  org    = org_pack ^. _OrganizationPackResponse .. organization_ ^. _OrganizationResponse
  forum  = forum_pack ^. _ForumPackResponse .. forum_ ^. _ForumResponse
  board  = board_pack ^. _BoardPackResponse .. board_ ^. _BoardResponse
  thread = thread_pack ^. _ThreadPackResponse .. thread_ ^. _ThreadResponse




renderPosts :: String -> String -> String -> String -> State -> ComponentHTML Input
renderPosts org_name forum_name board_name thread_name st =
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
                H.div [P.class_ B.colSm2] [
                    H.p_ [linkTo (Users (Show (usersMapLookup_ToNick st post.userId)) []) (usersMapLookup_ToNick st post.userId)]
                  , renderGravatarForUser Medium (usersMapLookup_ToUser st post.userId)
                  , displayUserStats (usersMapLookup st post.userId)
                ]
              , H.div [P.class_ B.colSm9] [
                    linkTo (OrganizationsForumsBoardsThreadsPosts org_name forum_name board_name thread_name (Show $ show post.id)) (thread_name <> "/" <> show post.id)
                  , H.p_ [H.text $ show post.createdAt]
                  , H.p_ [H.text "quote / reply"]
                  , displayPostData post.body
                  , H.p_ [H.text $ maybe "" (\user -> maybe "" id $ user ^. _UserSanitizedPackResponse .. profile_ ^. _ProfileResponse .. signature_) (usersMapLookup st post.userId)]
                ]
              , H.div [P.class_ B.colSm1] [
                    renderLike Ent_ThreadPost post.id pack'.like pack'.star
                  , displayPostStats (ThreadPostStatResponse stats)
                ]
            ]

          ]
        ) $ listToArray $ M.values st.threadPosts)
        <>
        -- INPUT FORM AT THE BOTTOM
        [H.li_ [
          H.div [P.class_ B.row] [
              H.div [P.class_ B.colSm2] [
                H.p_ [H.text "1"]
              ]
            , H.div [P.class_ B.colSm9] [
                H.p_ [H.text "2"]
              , H.div [P.class_ B.well] [
                -- TODO FIXME , need to fix this input form, doesnt do anything
                     H.a [P.href "#"] [H.text "Bold"]
                  ,  H.a [P.href "#"] [H.text "Youtube"]
                  ,  H.textarea [
                       P.class_ B.formControl,
                       P.rows 10,
                       P.value body,
                       E.onValueChange $ E.input (\v -> CompThreadPost (InputThreadPost_SetBody $ Just v))
                     ]
                -- TODO FIXME , need to style these buttons properly etc
                  , H.a [
                      P.classes [B.btn, B.btnPrimary, B.pullRight, B.btnSm],
                      E.onClick $ E.input (\v -> CompThreadPost InputThreadPost_Post)
                    ] [H.text "send"]
                  , H.a [
                      P.classes [B.btn, B.btnPrimary, B.pullRight, B.btnSm],
                      E.onClick $ E.input (\v -> CompThreadPost (InputThreadPost_SetBody Nothing))
                    ] [H.text "cancel"]
                  , H.a [P.classes [B.btn, B.btnPrimary, B.pullRight, B.btnSm]] [H.text "preview"]
                ]
              ]
            , H.div [P.class_ B.colSm1] [
                H.p_ [H.text "3"]
              ]
         ]
        ]])
  , renderPageNumbers st.threadPostsPageInfo st.currentPage
  ]
  where
  body = maybe "" (\p -> postDataToBody $ p ^. _ThreadPostRequest .. body_) st.currentThreadPost



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



postDataToBody :: PostData -> String
postDataToBody (PostDataBBCode v) = v
postDataToBody _                  = ""



displayPostData :: PostData -> ComponentHTML Input
displayPostData (PostDataBBCode v) = H.pre_ [H.text v]
displayPostData _                  = H.p_ [H.text "unknown post body"]
