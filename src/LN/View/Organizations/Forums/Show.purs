module LN.View.Organizations.Forums.Show (
  renderView_Organizations_Forums_Show
) where



import Daimyo.Data.ArrayList           (listToArray)
import Data.Map                        as M
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

  case st.currentOrganization, st.currentForum of

       Just org_pack, Just forum_pack -> renderView_Organizations_Forums_Show' org_pack forum_pack st
       _, _                           -> H.div_ [H.text "Unavailable"]



renderView_Organizations_Forums_Show' :: OrganizationPackResponse -> ForumPackResponse -> State -> ComponentHTML Input
renderView_Organizations_Forums_Show' org_pack forum_pack st =
  H.div [P.class_ B.containerFluid] [
    H.div [P.class_ B.pageHeader] [
        H.h2_ [H.text forum.name]
      , H.p [P.class_ B.lead] [H.text forum_desc]
    ],
    H.div [] [renderBoards org.name forum.name st]
  ]
  where
  org        = org_pack ^. _OrganizationPackResponse .. organization_ ^. _OrganizationResponse
  forum      = forum_pack ^. _ForumPackResponse .. forum_ ^. _ForumResponse
  forum_desc = maybe "No description." id forum.description



renderBoards :: String -> String -> State -> ComponentHTML Input
renderBoards org_name forum_name st =
  H.ul [P.class_ B.listUnstyled] $
    map (\pack ->
      let
        board_pack = pack ^. _BoardPackResponse
        board      = pack ^. _BoardPackResponse .. board_ ^. _BoardResponse
        stat       = pack ^. _BoardPackResponse .. boardStat_ ^. _BoardStatResponse
        thread     = pack ^. _BoardPackResponse .. latestThread_
        post       = pack ^. _BoardPackResponse .. latestThreadPost_
        user       = pack ^. _BoardPackResponse .. latestThreadPostUser_
      in
      H.li_ [
        H.div [P.class_ B.row] [
            H.div [P.class_ B.colSm1] [
                H.p_ [H.text "icon"]
            ]
          , H.div [P.class_ B.colSm5] [
                linkToP [] (OrganizationsForumsBoards org_name forum_name (Show $ board.name) []) board.name
              , H.p_ [H.text $ maybe "No description." id board.description]
            ]
          , H.div [P.class_ B.colSm1] [
              H.p_ [H.text $ show stat.threads <> " threads"]
            ]
          , H.div [P.class_ B.colSm1] [
              H.p_ [H.text $ show stat.threadPosts <> " posts"]
            ]
          , H.div [P.class_ B.colSm1] [
              H.p_ [H.text $ show stat.views <> " views"]
            ]
          , H.div [P.class_ B.colSm3] [
              case thread, post, user of
                   Just (ThreadResponse thread'), Just (ThreadPostResponse post'), Just (UserSanitizedResponse user') ->
                    H.div_ [
                      H.p_ [H.text $ "Last post by ", linkToP [] (Users (Show user'.nick) []) user'.nick],
                      H.p_ [H.text $ "in ", linkToP [Offset (-1)] (OrganizationsForumsBoardsThreads org_name forum_name board.name (Show thread'.name) []) thread'.name],
                      H.p_ [H.text $ show post'.createdAt]
                    ]
                   _, _, _ ->
                    H.div_ [
                      H.p_ [H.text "No posts."]
                    ]
          ]
        ]
      ])
    $ listToArray $ M.values st.boards
