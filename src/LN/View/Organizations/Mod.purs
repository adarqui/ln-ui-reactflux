module LN.View.Organizations.Mod (
  renderView_Organizations_Delete,
  renderView_Organizations_New,
  renderView_Organizations_Edit,
  renderView_Organizations_Mod
) where



import Data.Maybe                      (Maybe(..), maybe)
import Data.Tuple                      (Tuple(..))
import Halogen                         (ComponentHTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Events             as E
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3       as B
import Optic.Core                      ((^.), (..))
import Prelude                         (id, map, show, const, ($), (<<<))

import LN.Halogen.Util
import LN.Helpers.Array                (seqArrayFrom)
import LN.Helpers.JSON                 (decodeString)
import LN.Input.Organization           ()
import LN.Input.Types                  (Input(..), cOrganizationMod)
import LN.State.Loading                (getLoading, l_currentOrganization)
import LN.State.Organization           (OrganizationRequestState)
import LN.State.Types                  (State)
import LN.View.Module.Loading          (renderLoading)
import LN.T



renderView_Organizations_Delete :: Int -> State -> ComponentHTML Input
renderView_Organizations_Delete organization_id st =

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
    _, _, true                         -> renderLoading
    Just organization_req, Just o_st, false -> renderView_Organizations_Mod' m_organization_id organization_req o_st st
    _, _, false                        -> H.div_ [H.p_ [H.text "Organizations_Mod: unexpected error."]]



renderView_Organizations_Mod' :: Maybe Int -> OrganizationRequest -> OrganizationRequestState -> State -> ComponentHTML Input
renderView_Organizations_Mod' m_organization_id organization_req o_st st =
  H.div_ [

    H.h1_ [ H.text "Add Organization" ]

--  , input_Label "Title" "Title" organization.title P.InputText (E.input (cOrganizationMod <<< Organization_Mod_SetTitle))

--  , textArea_Label "Description" "Description" organization.description (E.input (cOrganizationMod <<< Organization_Mod_SetDescription))

  --
-- , simpleInfoButton save (cOrganizationMod $ Organization_Mod_Save m_organization_id)

  ]
  where
  organization = unwrapOrganizationRequest organization_req
  save     = maybe "Create" (const "Save") m_organization_id
