module LN.View.Organizations.Show (
  renderView_Organizations_Show
) where



import LN.ArrayList           (listToArray)
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
import LN.Input.Types                  (Input)
import LN.Router.Link                  (linkToP_Classes, linkToP_Glyph', linkToP)
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.Router.Class.Params          (emptyParams)
import LN.State.Loading                (getLoading, l_currentOrganization)
import LN.State.Types                  (State)
import LN.View.Helpers
import LN.View.Module.Loading          (renderLoading)
import LN.View.Forums.Index            (renderView_Forums_Index')
import LN.T                            ( OrganizationPackResponse
                                       , _OrganizationPackResponse, _OrganizationResponse, organization_
                                       , ForumPackResponse
                                       , _ForumPackResponse, _ForumResponse, forum_)



renderView_Organizations_Show :: State -> ComponentHTML Input
renderView_Organizations_Show st =

  case st.currentOrganization, getLoading l_currentOrganization st.loading of
       _, true              -> renderLoading
       Just org_pack, false -> renderView_Organizations_Show' org_pack st.forums
       _, _                 -> H.div_ [H.text "organization unavailable"]



renderView_Organizations_Show' :: OrganizationPackResponse -> M.Map Int ForumPackResponse -> ComponentHTML Input
renderView_Organizations_Show' org_pack forum_packs =
  H.div [P.class_ B.containerFluid] [
    H.div [P.class_ B.pageHeader] [
      H.h1 [P.class_ B.textCenter] [ H.text organization.name ],
      H.p [P.class_ B.textCenter] [ H.text $ maybe "" id organization.description ],

        orgMemberHTML
          org_pack
          unitDiv
          (\_ -> glyphButtonLinkDef_Ok $ OrganizationsMembership organization.name Index emptyParams),

        permissionsHTML'
          org_pack'.permissions
          permCreateEmpty
          permReadEmpty
          (\_ -> glyphButtonLinkDef_Pencil $ Organizations (Edit organization.name) emptyParams)
          (\_ -> glyphButtonLinkDef_Trash $ Organizations (Delete organization.name) emptyParams)
          permExecuteEmpty

--        if org_owner
--           then
--             buttonGroup_HorizontalSm1 [
--               glyphButtonLinkDef_Pencil $ Organizations (Edit organization.name) emptyParams,
--               glyphButtonLinkDef_Trash $ Organizations (Delete organization.name) emptyParams
--             ]
--           else H.div_ []

    ],
    H.div [P.class_ B.pageHeader] [
      H.p_ [ H.h4_ [H.text "Name:", H.small_ [H.text $ " " <> organization.name]]],
      ebyam organization.description (H.div_ []) $ \desc -> H.p_ [ H.h4_ [H.text "Description:", H.small_ [H.text desc]]],
      H.p_ [ H.h4_ [H.text "Company:", H.small_ [H.text $ " " <> organization.company]]],
      H.p_ [ H.h4_ [H.text "Location:", H.small_ [H.text $ " " <> organization.location]]],
      H.p_ [ H.h4_ [H.text "Membership:", H.small_ [H.text $ show organization.membership]]],
      H.p_ [ H.h4_ [H.text "Visibility:", H.small_ [H.text $ show organization.visibility]]],
      H.p_ [ H.h4_ [H.text "Tags:", showTagsSmall organization.tags]]
    ],
    renderView_Forums_Index' org_pack forum_packs,
    H.p_ [ H.h4_ [H.text "Members"]],
    H.p_ [ H.h4_ [H.text "teams"]],
    H.p_ [ H.h4_ [H.text "activity"]],
    H.p_ [ H.h4_ [H.text "stats"]]
  ]
  where
  organization = org_pack ^. _OrganizationPackResponse .. organization_ ^. _OrganizationResponse
  org_pack'    = org_pack ^. _OrganizationPackResponse
