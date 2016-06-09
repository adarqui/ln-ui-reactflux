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
import LN.Router.Link                  (linkToP, linkToP_Classes, linkToP_Glyph')
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.State.Types                  (State)
import LN.View.Boards.Show             (renderView_Boards_Show)
import LN.View.Module.Loading          (renderLoading)
import LN.T                            ( Param(..)
                                       , OrganizationPackResponse, ForumPackResponse
                                       , _OrganizationPackResponse, _OrganizationResponse, organization_
                                       , _ForumPackResponse, _ForumResponse, forum_
                                       , _BoardPackResponse, _BoardResponse, board_
                                       , _BoardStatResponse, stat_
                                       , ThreadResponse(..)
                                       , ThreadPostResponse(..)
                                       , UserSanitizedResponse(..)
                                       , latestThread_, latestThreadPost_, latestThreadPostUser_)



renderView_Organizations_Forums_Show :: String -> State -> ComponentHTML Input
renderView_Organizations_Forums_Show forum_name st =

  case st.currentOrganization, st.currentForum of

       Just org_pack, Just forum_pack -> renderView_Organizations_Forums_Show' org_pack forum_pack st
       _,             _               -> renderLoading



renderView_Organizations_Forums_Show'
  :: OrganizationPackResponse -> ForumPackResponse -> State -> ComponentHTML Input
renderView_Organizations_Forums_Show' org_pack forum_pack st =
  H.div [P.class_ B.containerFluid] [

    H.div [P.class_ B.pageHeader] [
      H.h2_ [H.text forum.name],
      linkToP [] (OrganizationsForums org.name (EditI forum.id) []) "edit",
      H.p [P.class_ B.lead] [H.text forum_desc]
    ],

    H.div [P.class_ B.container] [
      H.div_ [linkToP [] (OrganizationsForumsBoards org.name forum.name New []) "add-board"]
    ],

    H.div [] [renderView_Boards_Show st]
  ]
  where
  org        = org_pack ^. _OrganizationPackResponse .. organization_ ^. _OrganizationResponse
  forum      = forum_pack ^. _ForumPackResponse .. forum_ ^. _ForumResponse
  forum_desc = maybe "No description." id forum.description
