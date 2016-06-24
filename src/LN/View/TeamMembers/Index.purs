module LN.View.TeamMembers.Index (
  renderView_TeamMembers_Index,
  renderView_TeamMembers_Index'
) where



import LN.ArrayList                    (listToArray)
import Data.Map                        as M
import Data.Maybe                      (Maybe(..), maybe)
import Halogen                         (ComponentHTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3       as B
import Optic.Core                      ((^.), (..))
import Prelude                         (id, map, show, ($), (<>), (/=))

import LN.Access                       (permissionsHTML', unitDiv)
import LN.Input.Types                  (Input)
import LN.Router.Link                  (linkToP_Classes, linkToP_Glyph', linkToP)
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.Router.Class.Params          (emptyParams)
import LN.State.Types                  (State)
import LN.View.Helpers
import LN.View.Module.Loading          (renderLoading)
import LN.T                            ( TeamPackResponse(..), TeamResponse(..)
                                       , TeamMemberPackResponse
                                       , _TeamMemberPackResponse, _TeamMemberResponse
                                       , _TeamMemberPackResponse, _TeamMemberResponse, teamMember_
                                       , _TeamPackResponse, _TeamResponse, team_
                                       , OrganizationPackResponse, OrganizationResponse
                                       , _OrganizationPackResponse, _OrganizationResponse
                                       , organization_)



renderView_TeamMembers_Index :: State -> ComponentHTML Input
renderView_TeamMembers_Index st =

  case st.currentOrganization, st.currentTeam of
       Just org_pack, Just team_pack -> renderView_TeamMembers_Index' org_pack team_pack st.teamMembers
       _, _                          -> renderLoading



renderView_TeamMembers_Index' :: OrganizationPackResponse -> TeamPackResponse -> M.Map Int TeamMemberPackResponse -> ComponentHTML Input
renderView_TeamMembers_Index' org_pack team_pack team_member_packs =
  H.div [P.class_ B.pageHeader] [
    H.h1 [P.class_ B.textCenter] [ H.text "TeamMembers" ],

    H.div [P.class_ B.listUnstyled] $
      map (\team_member_pack ->
        let team_member = team_member_pack ^. _TeamMemberPackResponse .. teamMember_ ^. _TeamMemberResponse in
        H.li_ [
          linkToP [] (OrganizationsTeamsMembers organization.name (show team.system) (ShowI team_member.id) emptyParams) (show team_member.id)
        ]
      ) $ listToArray $ M.values team_member_packs

  ]
  where
  organization = org_pack ^. _OrganizationPackResponse .. organization_ ^. _OrganizationResponse
  team         = team_pack ^. _TeamPackResponse .. team_ ^. _TeamResponse
