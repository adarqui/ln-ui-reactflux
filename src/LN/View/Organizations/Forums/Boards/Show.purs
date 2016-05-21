module LN.View.Organizations.Forums.Boards.Show (
  renderView_Organizations_Forums_Boards_Show
) where



import Daimyo.Data.ArrayList           (listToArray)
import Data.Map                        as M
import Data.Maybe                      (Maybe(..), maybe)
import Halogen                         (ComponentHTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3       as B
import Optic.Core                      ((^.), (..))
import Prelude                         (id, show, map, ($), (<>))

import LN.Input.Types                  (Input)
import LN.Router.Link                  (linkToP)
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.State.Types                  (State)
import LN.State.User                   (usersMapLookup_ToUser)
import LN.View.Module.CreateThread     (renderCreateThread)
import LN.View.Module.Gravatar         (renderGravatarForUser)
import LN.View.Module.OrderBy          (renderOrderBy)
import LN.View.Module.PageNumbers      (renderPageNumbers)
import LN.T                            (BoardPackResponse, ForumPackResponse, OrganizationPackResponse
                                       , Size(Small), ThreadPostResponse(ThreadPostResponse)
                                       , UserSanitizedResponse(UserSanitizedResponse), latestThreadPostUser_
                                       , _ThreadPackResponse, latestThreadPost_, _ThreadStatResponse, stat_
                                       , _ThreadResponse, thread_, _BoardResponse, board_, _BoardPackResponse
                                       , _ForumResponse, forum_, _ForumPackResponse, _OrganizationResponse
                                       , organization_, _OrganizationPackResponse)



renderView_Organizations_Forums_Boards_Show :: String -> State -> ComponentHTML Input
renderView_Organizations_Forums_Boards_Show board_name st =

  case st.currentOrganization, st.currentForum, st.currentBoard of

       Just org_pack, Just forum_pack, Just board_pack ->
         renderView_Organizations_Forums_Boards_Show' org_pack forum_pack board_pack st

       _,             _,               _               -> H.div_ [H.text "Unavailable"]



renderView_Organizations_Forums_Boards_Show'
  :: OrganizationPackResponse -> ForumPackResponse -> BoardPackResponse -> State -> ComponentHTML Input
renderView_Organizations_Forums_Boards_Show' org_pack forum_pack board_pack st =
  H.div [P.class_ B.containerFluid] [
    H.div [P.class_ B.pageHeader] [
        H.h2_ [H.text board.name]
      , H.p [P.class_ B.lead] [H.text board_desc]
    ],
    H.div [P.class_ B.clearfix] [H.span [P.classes [B.pullLeft]] [renderOrderBy st.currentPage]],
    H.div [P.class_ B.clearfix] [H.span [P.classes [B.pullRight]] [renderCreateThread st.compCreateThread]],
    H.div [] [renderThreads org.name forum.name board.name st]
  ]
  where
  org        = org_pack ^. _OrganizationPackResponse .. organization_ ^. _OrganizationResponse
  forum      = forum_pack ^. _ForumPackResponse .. forum_ ^. _ForumResponse
  board      = board_pack ^. _BoardPackResponse .. board_ ^. _BoardResponse
  board_desc = maybe "No description." id board.description



renderThreads :: String -> String -> String -> State -> ComponentHTML Input
renderThreads org_name forum_name board_name st =
  H.div_ [
      renderPageNumbers st.threadsPageInfo st.currentPage
    , H.ul [P.class_ B.listUnstyled] $
        map (\pack ->
          let
            thread_pack = pack ^. _ThreadPackResponse
            thread      = pack ^. _ThreadPackResponse .. thread_ ^. _ThreadResponse
            stat        = pack ^. _ThreadPackResponse .. stat_ ^. _ThreadStatResponse
            post        = pack ^. _ThreadPackResponse .. latestThreadPost_
            user        = pack ^. _ThreadPackResponse .. latestThreadPostUser_
          in
          H.li_ [
            H.div [P.class_ B.row] [
                H.div [P.class_ B.colSm1] [
                  renderGravatarForUser Small (usersMapLookup_ToUser st thread.userId)
                ]
              , H.div [P.class_ B.colSm6] [
                    linkToP [] (OrganizationsForumsBoardsThreads org_name forum_name board_name (Show thread.name) []) thread.name
                  , H.p_ [H.text "page-numbers"]
                  , H.p_ [H.text $ show thread.createdAt]
                ]
              , H.div [P.class_ B.colSm1] [
                  H.p_ [H.text $ show stat.threadPosts <> " posts"]
                ]
              , H.div [P.class_ B.colSm1] [
                  H.p_ [H.text $ show stat.views <> " views"]
                ]
              , H.div [P.class_ B.colSm3] [
                case post, user of
                     Just (ThreadPostResponse post'), Just (UserSanitizedResponse user') ->
                       H.div_ [
                         H.p_ [H.text $ user'.nick],
                         H.p_ [H.text $ show post'.createdAt]
                       ]
                     _, _ -> H.div_ [ H.p_ [H.text "No posts."]]
              ]
            ]
          ])
        $ listToArray $ M.values st.threads
    , renderPageNumbers st.threadsPageInfo st.currentPage
  ]
