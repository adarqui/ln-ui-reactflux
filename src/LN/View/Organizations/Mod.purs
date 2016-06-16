module LN.View.Organizations.Mod (
  renderView_Organizations_Delete,
  renderView_Organizations_New,
  renderView_Organizations_Edit,
  renderView_Organizations_Mod,

  renderView_Organizations_DeleteS,
  renderView_Organizations_NewS,
  renderView_Organizations_EditS
) where



import Data.Maybe                      (Maybe(..))
import Halogen                         (ComponentHTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Events             as E
import Halogen.HTML.Properties.Indexed as P
import Optic.Core                      ((^.), (..))
import Prelude                         ((<<<))

import LN.Halogen.Util                 (input_Label)
import LN.Input.Organization           (Organization_Mod(..))
import LN.Input.Types                  (Input, cOrganizationMod)
import LN.Router.Class.Routes          (Routes(..))
import LN.State.Loading                (getLoading, l_currentOrganization)
import LN.State.Organization           (OrganizationRequestState)
import LN.State.Types                  (State)
import LN.View.Helpers                 (buttons_CreateEditCancel)
import LN.View.Fields                  ( mandatoryNameField, optionalDescriptionField, mandatoryCompanyField, mandatoryLocationField
                                       , mandatoryMembershipField
                                       )
import LN.View.Module.Loading          (renderLoading)
import LN.T



renderView_Organizations_Delete :: State -> ComponentHTML Input
renderView_Organizations_Delete st =

  case st.currentOrganization, getLoading l_currentOrganization st.loading of
       _, true          -> renderLoading
       Nothing, false   -> H.div_ [H.p_ [H.text "organization unavailable."]]
       Just pack, false -> renderView_Organizations_Delete' pack st



renderView_Organizations_Delete' :: OrganizationPackResponse -> State -> ComponentHTML Input
renderView_Organizations_Delete' pack st =
  H.div_ [H.p_ [H.text "Delete? <yes/no>"]]
 where
 organization = pack ^. _OrganizationPackResponse .. organization_ ^. _OrganizationResponse



renderView_Organizations_New :: State -> ComponentHTML Input
renderView_Organizations_New = renderView_Organizations_Mod Nothing



renderView_Organizations_Edit :: Int -> State -> ComponentHTML Input
renderView_Organizations_Edit organization_id = renderView_Organizations_Mod (Just organization_id)



renderView_Organizations_Mod :: Maybe Int -> State -> ComponentHTML Input
renderView_Organizations_Mod m_organization_id st =
  case st.currentOrganizationRequest, st.currentOrganizationRequestSt, getLoading l_currentOrganization st.loading of
    _, _, true                              -> renderLoading
    Just organization_req, Just o_st, false -> renderView_Organizations_Mod' m_organization_id organization_req o_st st
    _, _, false                             -> H.div_ [H.p_ [H.text "Organizations_Mod: unexpected error."]]



renderView_Organizations_Mod' :: Maybe Int -> OrganizationRequest -> OrganizationRequestState -> State -> ComponentHTML Input
renderView_Organizations_Mod' m_organization_id organization_req o_st st =
  H.div_ [

    H.h1_ [ H.text "Add Organization" ]

  , mandatoryNameField organization.displayName (cOrganizationMod <<< SetDisplayName)

  , optionalDescriptionField organization.description (cOrganizationMod <<< SetDescription) (cOrganizationMod RemoveDescription)

  , mandatoryCompanyField organization.company (cOrganizationMod <<< SetCompany)

  , mandatoryLocationField organization.location (cOrganizationMod <<< SetLocation)

  -- , membership
  -- , visibility
  -- , tags
  -- , icon

  , buttons_CreateEditCancel m_organization_id (cOrganizationMod Create) (cOrganizationMod <<< EditP) About

  ]
  where
  organization = unwrapOrganizationRequest organization_req



renderView_Organizations_DeleteS :: State -> ComponentHTML Input
renderView_Organizations_DeleteS = renderView_Organizations_Delete



renderView_Organizations_NewS :: State -> ComponentHTML Input
renderView_Organizations_NewS = renderView_Organizations_New



renderView_Organizations_EditS :: State -> ComponentHTML Input
renderView_Organizations_EditS st =

  case st.currentOrganization of

    Just org_pack ->
      renderView_Organizations_Edit
        (org_pack ^. _OrganizationPackResponse .. organization_ ^. _OrganizationResponse .. id_)
        st

    _            -> H.div_ [H.text "error"]
