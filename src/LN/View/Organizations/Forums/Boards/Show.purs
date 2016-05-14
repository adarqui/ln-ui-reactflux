module LN.View.Organizations.Forums.Boards.Show (
  renderView_Organizations_Forums_Boards_Show
) where



import Data.Maybe                      (Maybe(..), maybe)
import Halogen                         (ComponentHTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3       as B
import Optic.Core                      ((^.), (..))
import Prelude                         (id, show, map, negate, ($), (<>))

import LN.Input.Types                  (Input)
import LN.Router.Internal              (linkToP)
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.State.Types                  (State)
import LN.State.User                   (usersMapLookup_ToUser)
import LN.View.Module.CreateThread
import LN.View.Module.Gravatar
import LN.View.Module.OrderBy
import LN.View.Module.PageNumbers
import LN.T



renderView_Organizations_Forums_Boards_Show :: String -> State -> ComponentHTML Input
renderView_Organizations_Forums_Boards_Show board_name st =
  H.div [P.class_ B.containerFluid] [
    H.div [P.class_ B.pageHeader] [
        H.h2_ [H.text board_name]
      , H.p [P.class_ B.lead] [H.text board_desc]
    ],
    H.div [P.class_ B.clearfix] [H.span [P.classes [B.pullLeft]] [renderOrderBy st.currentPage]],
    H.div [P.class_ B.clearfix] [H.span [P.classes [B.pullRight]] [renderCreateThread st.compCreateThread]],
    H.div [] [threads org_name forum_name board_name st]
  ]
  where
  org_name = maybe "Empty" (\org -> org ^. _OrganizationResponse .. name_) st.currentOrganization
  forum_name = maybe "Empty" (\forum -> forum ^. _ForumResponse .. name_) st.crrentForum
  board_desc = maybe "No description." (\board -> maybe "No description." id (board ^. _BoardResponse .. description_)) st.currentBoard



threads :: String -> String -> String -> State -> ComponentHTML Input
threads org_name forum_name board_name st =
  H.div_ [
      renderPageNumbers st.threadsPageInfo st.currentPage
    , H.ul [P.class_ B.listUnstyled] $
        map (\(pack@(ThreadPackResponse thread_pack)) ->
          H.li_ [
            H.div [P.class_ B.row] [
                H.div [P.class_ B.colSm1] [
                  renderGravatarForUser Small (usersMapLookup_ToUser st (t pack ^. userId_))
                ]
              , H.div [P.class_ B.colSm6] [
                    linkToP [] (OrganizationsForumsBoardsThreads org_name forum_name board_name (Show $ t pack ^. name_) []) (t pack ^. name_)
                  , H.p_ [H.text "page-numbers"]
                  , H.p_ [H.text $ show $ t pack ^. createdAt_]
                ]
              , H.div [P.class_ B.colSm1] [
                  H.p_ [H.text $ show (ts pack ^. threadPosts_) <> " posts"]
                ]
              , H.div [P.class_ B.colSm1] [
                  H.p_ [H.text $ show (ts pack ^. views_) <> " views"]
                ]
              , H.div [P.class_ B.colSm3] [
                case ({ _tp: tp pack, _tpu: tpu pack }) of
                     { _tp: Just thread_post, _tpu: Just user } ->
                       H.div_ [
                         H.p_ [H.text $ user ^. _UserSanitizedResponse .. nick_],
                         H.p_ [H.text $ show $ thread_post ^. _ThreadPostResponse .. createdAt_]
--                         H.p_ [H.text $ show $ thread_post ^. _ThreadPostResponse .. createdAt_]
                       ]
                     _ -> H.div_ [ H.p_ [H.text "No posts."]]
              ]
            ]
          ])
        st.threads
    , renderPageNumbers (st ^. stThreadsPageInfo) st.currentPage
  ]
  where
  -- thread
  t x = (x ^. _ThreadPackResponse .. thread_ ^. _ThreadResponse)
  -- thread user
  tu x = (x ^. _ThreadPackResponse .. threadUser_ ^. _UserSanitizedResponse)
  -- thread stat
  ts x = (x ^. _ThreadPackResponse .. threadStat_ ^. _ThreadStatResponse)
  -- thread post
  tp x = (x ^. _ThreadPackResponse .. latestThreadPost_)
  -- thread post user
  tpu x = (x ^. _ThreadPackResponse .. latestThreadPostUser_)
