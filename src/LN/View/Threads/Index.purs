module LN.View.Threads.Index (
  renderView_Threads_Index,
  renderView_Threads_Index'
) where



import LN.ArrayList                    (listToArray)
import Data.Map                        as M
import Data.Maybe                      (Maybe(..), maybe)
import Halogen                         (ComponentHTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3       as B
import Optic.Core                      ((^.), (..))
import Prelude                         (id, show, map, ($), (<>), (||), (==))

import LN.Access                       (permissionsHTML', unitDiv)
import LN.Input.Types                  (Input)
import LN.Router.Link                  (linkToP, linkToP_Classes)
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.Router.Class.Params          (emptyParams)
import LN.State.PageInfo               (PageInfo)
import LN.State.Types                  (State)
import LN.State.User                   (usersMapLookup_ToUser')
-- import LN.View.Module.CreateThread     (renderCreateThread)
import LN.View.Helpers
import LN.View.Module.Gravatar         (renderGravatarForUser)
import LN.View.Module.Loading          (renderLoading)
import LN.View.Module.OrderBy          (renderOrderBy)
import LN.View.Module.PageNumbers      (renderPageNumbers)
import LN.T                            (BoardPackResponse, ForumPackResponse, OrganizationPackResponse
                                       , Size(Small), ThreadPostResponse(ThreadPostResponse)
                                       , UserSanitizedResponse(UserSanitizedResponse), latestThreadPostUser_
                                       , ThreadPackResponse
                                       , UserSanitizedPackResponse
                                       , _ThreadPackResponse, latestThreadPost_, _ThreadStatResponse, stat_
                                       , _ThreadResponse, thread_, _BoardResponse, board_, _BoardPackResponse
                                       , _ForumResponse, forum_, _ForumPackResponse, _OrganizationResponse
                                       , organization_, _OrganizationPackResponse
                                       )



renderView_Threads_Index :: State -> ComponentHTML Input
renderView_Threads_Index st =

  case st.currentOrganization, st.currentForum, st.currentBoard of

       Just org_pack, Just forum_pack, Just board_pack ->

         renderView_Threads_Index' st.meId org_pack forum_pack board_pack st.threads st.threadsPageInfo st.currentPage st.usersMap

       _,             _,               _               -> renderLoading



renderView_Threads_Index'
  :: Int
  -> OrganizationPackResponse
  -> ForumPackResponse
  -> BoardPackResponse
  -> M.Map Int ThreadPackResponse
  -> PageInfo
  -> Routes
  -> M.Map Int UserSanitizedPackResponse
  -> ComponentHTML Input
renderView_Threads_Index' me_id org_pack forum_pack board_pack thread_packs threads_page_info threads_route users_map =
  H.div_ [
      renderPageNumbers threads_page_info threads_route

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
                  renderGravatarForUser Small (usersMapLookup_ToUser' users_map thread.userId)
                ]
              , H.div [P.class_ B.colXs5] [
                    H.div [P.class_ B.listGroup] [linkToP_Classes [B.listGroupItem] [] (OrganizationsForumsBoardsThreads org.name forum.name board.name (Show thread.name) emptyParams) thread.displayName]
                  , H.p_ [H.text "page-numbers"]
                  , H.p_ [H.text $ show thread.createdAt]
                ]
              , H.div [P.class_ B.colXs2] [
                  showBadge' "posts " stat.threadPosts,
                  showBadge' "views " stat.views
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
              , H.div [P.class_ B.colXs1] [
                  buttonGroup_VerticalSm1 [
--                    permissionsHTML' board_pack'.permissions unitDiv
--                   glyphButtonLinkDef_Pencil $ OrganizationsForumsBoardsThreads org.name forum.name board.name (Edit thread.name) emptyParams,
--                   glyphButtonLinkDef_Trash $ OrganizationsForumsBoardsThreads org.name forum.name board.name (Delete thread.name) emptyParams
                  ]
                ]
            ]
          ])
        $ listToArray $ M.values thread_packs
    , renderPageNumbers threads_page_info threads_route
  ]
  where
  org         = org_pack ^. _OrganizationPackResponse .. organization_ ^. _OrganizationResponse
  forum       = forum_pack ^. _ForumPackResponse .. forum_ ^. _ForumResponse
  board       = board_pack ^. _BoardPackResponse .. board_ ^. _BoardResponse
  board_pack' = board_pack ^. _BoardPackResponse
