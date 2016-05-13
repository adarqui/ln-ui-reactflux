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
import Prelude                         (id, show, map, negate, ($), (<>))

import LN.Input.Types                  (Input (..))
import LN.Input.ThreadPost             (InputThreadPost (..))
import LN.Router.Internal              (linkTo, linkToP)
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.State.Types                  (State)
import LN.State.Lens
import LN.View.Module.Gravatar
import LN.View.Module.LikeThreadPost
import LN.View.Module.PageNumbers
import LN.T



renderView_Organizations_Forums_Boards_Threads_Show :: String -> State -> ComponentHTML Input
renderView_Organizations_Forums_Boards_Threads_Show thread_name st =
  H.div [P.class_ B.containerFluid] [
    H.div [P.class_ B.pageHeader] [
        H.h2_ [H.text thread_name]
    ],
    H.div [] [posts org_name forum_name board_name thread_name st]
  ]
  where
  org_name = maybe "Empty" (\org -> org ^. _OrganizationResponse .. name_) (st ^. stCurrentOrganization)
  forum_name = maybe "Empty" (\forum -> forum ^. _ForumResponse .. name_) (st ^. stCurrentForum)
  board_name = maybe "Empty" (\board -> board ^. _BoardResponse .. name_) (st ^. stCurrentBoard)
  thread_name = maybe "Empty" (\thread -> thread ^. _ThreadResponse .. name_) (st ^. stCurrentThread)



posts :: String -> String -> String -> String -> State -> ComponentHTML Input
posts org_name forum_name board_name thread_name st =
  H.div_ [
      renderPageNumbers (st ^. stThreadPostsPageInfo) (st ^. stCurrentPage)
    , H.ul [P.class_ B.listUnstyled] (
        (map (\pack ->
          let
            post = pack ^. _ThreadPostPackResponse .. threadPost_
          in
          H.li_ [

            H.div [P.class_ B.row] [
                H.div [P.class_ B.colSm2] [
                    H.p_ [linkTo (Users $ Show (usersMapLookup_ToNick st (post_user_id post))) (usersMapLookup_ToNick st (post_user_id post))]
                  , renderGravatarForUser Medium (usersMapLookup_ToUser st (post_user_id post))
                  , displayUserStats (usersMapLookup st (post_user_id post))
                ]
              , H.div [P.class_ B.colSm9] [
                    linkTo (OrganizationsForumsBoardsThreadsPosts org_name forum_name board_name thread_name (Show $ show (post_id post))) (thread_name <> "/" <> show (post_id post))
                  , H.p_ [H.text $ show (post_created_at post)]
                  , H.p_ [H.text "quote / reply"]
                  , displayPostData (post_body post)
                  , H.p_ [H.text $ maybe "" (\user -> maybe "" id $ user ^. _UserSanitizedPackResponse .. userProfile_ ^. _ProfileResponse .. signature_) (usersMapLookup st (post_user_id post))]
                ]
              , H.div [P.class_ B.colSm1] [
                    H.p_ [H.text "likes / up / down"]
                  , renderLikeThreadPost (post_id post) pack
                  , H.p_ [H.text $ show $ likes_up pack]
                  , H.p_ [H.text $ show $ likes_down pack]
                  , H.p_ [H.text $ show $ starred pack]
                  , H.p_ [H.text $ show $ views pack ]
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
  , renderPageNumbers (st ^. stThreadPostsPageInfo) (st ^. stCurrentPage)
  ]
  where
  body = maybe "" (\p -> postDataToBody $ p ^. _ThreadPostRequest .. body_) (st ^. stCurrentThreadPost)
  post_user_id post    = post ^. _ThreadPostResponse .. userId_
  post_id post         = post ^. _ThreadPostResponse .. id_
  post_created_at post = post ^. _ThreadPostResponse .. createdAt_
  post_body post       = post ^. _ThreadPostResponse .. body_
  likes_up pack        = pack ^. _ThreadPostPackResponse .. stat_ ^. _ThreadPostStatResponse .. likes_
  likes_down pack      = pack ^. _ThreadPostPackResponse .. stat_ ^. _ThreadPostStatResponse .. dislikes_
  starred pack         = pack ^. _ThreadPostPackResponse .. stat_ ^. _ThreadPostStatResponse .. starred_
  views pack           = pack ^. _ThreadPostPackResponse .. stat_ ^. _ThreadPostStatResponse .. views_



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
  stats = user ^. _UserSanitizedPackResponse .. userStat_ ^. _UserSanitizedStatResponse



postDataToBody :: PostData -> String
postDataToBody (PostDataBBCode v) = v
postDataToBody _ = ""



displayPostData :: PostData -> ComponentHTML Input
displayPostData (PostDataBBCode v) = H.pre_ [H.text v]
displayPostData _                  = H.p_ [H.text "unknown post body"]
