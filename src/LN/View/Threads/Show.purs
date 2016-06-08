module LN.View.Threads.Show (
  renderView_Threads_Show,
  renderView_Threads_Show'
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
import LN.Router.Link                  (linkToP, linkToP_Classes)
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.State.Types                  (State)
import LN.State.User                   (usersMapLookup_ToUser)
-- import LN.View.Module.CreateThread     (renderCreateThread)
import LN.View.Module.Gravatar         (renderGravatarForUser)
import LN.View.Module.Loading          (renderLoading)
import LN.View.Module.OrderBy          (renderOrderBy)
import LN.View.Module.PageNumbers      (renderPageNumbers)
import LN.T                            (BoardPackResponse, ForumPackResponse, OrganizationPackResponse
                                       , Size(Small), ThreadPostResponse(ThreadPostResponse)
                                       , UserSanitizedResponse(UserSanitizedResponse), latestThreadPostUser_
                                       , _ThreadPackResponse, latestThreadPost_, _ThreadStatResponse, stat_
                                       , _ThreadResponse, thread_, _BoardResponse, board_, _BoardPackResponse
                                       , _ForumResponse, forum_, _ForumPackResponse, _OrganizationResponse
                                       , organization_, _OrganizationPackResponse)



renderView_Threads_Show :: State -> ComponentHTML Input
renderView_Threads_Show st =

  case st.currentOrganization, st.currentForum, st.currentBoard of

       Just org_pack, Just forum_pack, Just board_pack -> renderView_Threads_Show' org_pack forum_pack board_pack st
       _,             _,               _               -> renderLoading



renderView_Threads_Show' :: OrganizationPackResponse -> ForumPackResponse -> BoardPackResponse -> State -> ComponentHTML Input
renderView_Threads_Show' org_pack forum_pack board_pack st =
  H.div_ [
      renderPageNumbers st.threadsPageInfo st.currentPage

    , H.div_ [linkToP [] (OrganizationsForumsBoardsThreads org.name forum.name board.name New []) "new-thread"]

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
                H.div [P.class_ B.colXs1] [
                  renderGravatarForUser Small (usersMapLookup_ToUser st thread.userId),
                  H.div_ [linkToP [] (OrganizationsForumsBoardsThreads org.name forum.name board.name (EditI 0) []) "edit"],
                  H.div_ [linkToP [] (OrganizationsForumsBoardsThreads org.name forum.name board.name (DeleteI 0) []) "delete"]
                ]
              , H.div [P.class_ B.colXs6] [
                    H.div [P.class_ B.listGroup] [linkToP_Classes [B.listGroupItem] [] (OrganizationsForumsBoardsThreads org.name forum.name board.name (Show thread.name) []) thread.displayName]
                  , H.p_ [H.text "page-numbers"]
                  , H.p_ [H.text $ show thread.createdAt]
                ]
              , H.div [P.class_ B.colXs1] [
                  H.p_ [H.text $ show stat.threadPosts <> " posts"]
                ]
              , H.div [P.class_ B.colXs1] [
                  H.p_ [H.text $ show stat.views <> " views"]
                ]
              , H.div [P.class_ B.colXs3] [
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
  where
  org   = org_pack ^. _OrganizationPackResponse .. organization_ ^. _OrganizationResponse
  forum = forum_pack ^. _ForumPackResponse .. forum_ ^. _ForumResponse
  board = board_pack ^. _BoardPackResponse .. board_ ^. _BoardResponse
