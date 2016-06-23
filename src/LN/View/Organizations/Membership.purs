module LN.View.Organizations.Membership (
  renderView_Organizations_Membership_Index,
  renderView_Organizations_Membership_Index',
  renderView_Organizations_Membership_Delete,
  renderView_Organizations_Membership_Delete'
) where



import LN.ArrayList                    (listToArray)
import Data.Ebyam                      (ebyam)
import Data.Map                        as M
import Data.Maybe                      (Maybe(..), maybe)
import Halogen                         (ComponentHTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3       as B
import Optic.Core                      ((^.), (..))
import Prelude                         (id, map, show, ($), (<>))

import LN.Access
import LN.Input.Membership             (Membership_Act(..))
import LN.Input.Types                  (Input, cMembershipAct)
import LN.Router.Link                  (linkToP_Classes, linkToP_Glyph', linkToP)
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.Router.Class.Params          (emptyParams)
import LN.State.Loading                (getLoading, l_currentOrganization)
import LN.State.Types                  (State)
import LN.View.Helpers
import LN.View.Module.Loading          (renderLoading)
import LN.View.Organizations.MembersOnly (renderView_Organizations_MembersOnly_Already')
import LN.T                            ( OrganizationPackResponse
                                       , _OrganizationPackResponse, _OrganizationResponse, organization_
                                       , Membership(..))



renderView_Organizations_Membership_Index :: State -> ComponentHTML Input
renderView_Organizations_Membership_Index st =

  case st.currentOrganization, getLoading l_currentOrganization st.loading of
       _, true              -> renderLoading
       Just org_pack, false -> renderView_Organizations_Membership_Index' org_pack
       _, _                 -> H.div_ [H.text "organization unavailable"]



renderView_Organizations_Membership_Index' :: OrganizationPackResponse -> ComponentHTML Input
renderView_Organizations_Membership_Index' org_pack =
  H.div [P.class_ B.containerFluid] [
    H.div [P.class_ B.pageHeader] [
      H.h1 [P.class_ B.textCenter] [ H.text organization.name ]
    ],
    H.div [] [
      orgMemberHTML
        org_pack
        (\_ -> renderView_Organizations_MembersOnly_Already' org_pack)
        (\_ -> renderView_Organizations_Membership_Index'' org_pack)
    ]
  ]
  where
  organization = org_pack ^. _OrganizationPackResponse .. organization_ ^. _OrganizationResponse
  org_pack'    = org_pack ^. _OrganizationPackResponse



renderView_Organizations_Membership_Index'' :: OrganizationPackResponse -> ComponentHTML Input
renderView_Organizations_Membership_Index'' org_pack =
  H.div_ [
    case organization.membership of
         Membership_InviteOnly    -> H.p_ [H.text "You must be invited."]
         Membership_RequestInvite -> H.p_ [H.text "Request Invite"]
         Membership_Join          -> textButtonSm "Join" $ cMembershipAct Join_ByCurrentOrganization
         Membership_Locked        -> H.p_ [H.text "Sorry, this organization is locked."]
  ]
  where
  organization = org_pack ^. _OrganizationPackResponse .. organization_ ^. _OrganizationResponse
  org_pack'    = org_pack ^. _OrganizationPackResponse








-- | Delete means "LEAVE" organization
--
renderView_Organizations_Membership_Delete :: State -> ComponentHTML Input
renderView_Organizations_Membership_Delete st =

  case st.currentOrganization, getLoading l_currentOrganization st.loading of
       _, true              -> renderLoading
       Just org_pack, false -> renderView_Organizations_Membership_Delete' org_pack
       _, _                 -> H.div_ [H.text "organization unavailable"]



renderView_Organizations_Membership_Delete' :: OrganizationPackResponse -> ComponentHTML Input
renderView_Organizations_Membership_Delete' org_pack =
  H.div [P.class_ B.containerFluid] [
    H.div [P.class_ B.pageHeader] [
      H.h1 [P.class_ B.textCenter] [ H.text organization.name ]
    ],
    H.div [] [
      orgMemberHTML
        org_pack
        (\_ -> renderView_Organizations_Membership_Delete'' org_pack)
        (\_ -> renderView_Organizations_Membership_Index'' org_pack)
    ]
  ]
  where
  organization = org_pack ^. _OrganizationPackResponse .. organization_ ^. _OrganizationResponse
  org_pack'    = org_pack ^. _OrganizationPackResponse



renderView_Organizations_Membership_Delete'' :: OrganizationPackResponse -> ComponentHTML Input
renderView_Organizations_Membership_Delete'' org_pack =
  H.div_ [
    H.p_ [
      H.text "Are you sure you wish to leave this organization?",
      textButtonSm "Leave" $ cMembershipAct Leave_ByCurrentOrganization
    ]
  ]
  where
  organization = org_pack ^. _OrganizationPackResponse .. organization_ ^. _OrganizationResponse
  org_pack'    = org_pack ^. _OrganizationPackResponse
