module LN.View.Organizations.Forums.Show (
  renderView_Organizations_Forums_Show
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
import LN.T



renderView_Organizations_Forums_Show :: String -> State -> ComponentHTML Input
renderView_Organizations_Forums_Show forum_name st =
  H.div [P.class_ B.containerFluid] [
    H.div [P.class_ B.pageHeader] [
        H.h2_ [H.text forum_name]
      , H.p [P.class_ B.lead] [H.text forum_desc]
    ],
    H.div [] [render_boards_packs org_name forum_name st]
  ]
  where
  org_name = maybe "Empty" (\org -> org ^. _OrganizationResponse .. name_) st.currentOrganization
  forum_desc = maybe "No description." (\forum -> maybe "No description" id (forum ^. _ForumResponse .. description_)) (st ^. stCurrentForum)



render_boards_packs :: String -> String -> State -> ComponentHTML Input
render_boards_packs org_name forum_name st =
  H.ul [P.class_ B.listUnstyled] $
    map (\(bp@(BoardPackResponse board_pack)) ->
      H.li_ [
        H.div [P.class_ B.row] [
            H.div [P.class_ B.colSm1] [
                H.p_ [H.text "icon"]
            ]
          , H.div [P.class_ B.colSm5] [
                linkToP [] (OrganizationsForumsBoards org_name forum_name (Show $ b bp ^. name_) []) (b bp ^. name_)
              , H.p_ [H.text $ maybe "No description." id ((b bp) ^. description_)]
            ]
          , H.div [P.class_ B.colSm1] [
              H.p_ [H.text $ show (bs bp ^. threads_) <> " threads"]
            ]
          , H.div [P.class_ B.colSm1] [
              H.p_ [H.text $ show (bs bp ^. threadPosts_) <> " posts"]
            ]
          , H.div [P.class_ B.colSm1] [
              H.p_ [H.text $ show (bs bp ^. views_) <> " views"]
            ]
          , H.div [P.class_ B.colSm3] [
              case ({ _t: t bp, _tp: tp bp, _u: u bp}) of
                   { _t: Just (ThreadResponse t'), _tp: Just (ThreadPostResponse tp'), _u: Just (UserSanitizedResponse u') } ->
--              case (tuple3 (t bp) (tp bp) (u bp)) of
--                   (Just (ThreadResponse t')) /\ (Just (ThreadPostResponse tp')) /\ (Just (UserSanitizedResponse u')) ->
                    H.div_ [
                      H.p_ [H.text $ "Last post by ", linkToP [] (Users (Show u'.nick)) u'.nick],
                      H.p_ [H.text $ "in ", linkToP [Offset (-1)] (OrganizationsForumsBoardsThreads org_name forum_name (b bp ^. name_) (Show t'.name) []) t'.name],
                      H.p_ [H.text $ show tp'.createdAt]
                    ]
                   _ ->
                    H.div_ [
                      H.p_ [H.text "No posts."]
                    ]
          ]
        ]
      ])
    st.boards
  where
  -- board
  b x = (x ^. _BoardPackResponse .. board_ ^. _BoardResponse)
  -- board stat
  bs x = (x ^. _BoardPackResponse .. boardStat_ ^. _BoardStatResponse)
  -- thread
  t x = (x ^. _BoardPackResponse .. latestThread_)
  -- thread post
  tp x = (x ^. _BoardPackResponse .. latestThreadPost_)
  -- user
  u x = (x ^. _BoardPackResponse .. latestThreadPostUser_)
