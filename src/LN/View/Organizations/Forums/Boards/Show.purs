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
import LN.Router.Link                  (linkToP, linkToP_Classes)
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.State.Types                  (State)
import LN.State.User                   (usersMapLookup_ToUser)
-- import LN.View.Module.CreateThread     (renderCreateThread)
import LN.View.Threads.Show            (renderView_Threads_Show)
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



renderView_Organizations_Forums_Boards_Show :: String -> State -> ComponentHTML Input
renderView_Organizations_Forums_Boards_Show board_name st =

  case st.currentOrganization, st.currentForum, st.currentBoard of

       Just org_pack, Just forum_pack, Just board_pack ->
         renderView_Organizations_Forums_Boards_Show' org_pack forum_pack board_pack st

       _,             _,               _               -> renderLoading



renderView_Organizations_Forums_Boards_Show'
  :: OrganizationPackResponse -> ForumPackResponse -> BoardPackResponse -> State -> ComponentHTML Input
renderView_Organizations_Forums_Boards_Show' org_pack forum_pack board_pack st =
  H.div [P.class_ B.containerFluid] [
    H.div [P.class_ B.pageHeader] [
      H.h2_ [H.text board.name],
      H.p [P.class_ B.lead] [H.text board_desc],
      H.div_ [linkToP [] (OrganizationsForumsBoards org.name forum.name (EditI 0) []) "edit"],
      H.div_ [linkToP [] (OrganizationsForumsBoards org.name forum.name (EditI 0) []) "delete"]
    ],
    H.div [P.class_ B.clearfix] [H.span [P.classes [B.pullLeft]] [renderOrderBy st.currentPage]],
    H.div [] [renderView_Threads_Show st]
  ]
  where
  org        = org_pack ^. _OrganizationPackResponse .. organization_ ^. _OrganizationResponse
  forum      = forum_pack ^. _ForumPackResponse .. forum_ ^. _ForumResponse
  board      = board_pack ^. _BoardPackResponse .. board_ ^. _BoardResponse
  board_desc = maybe "No description." id board.description
