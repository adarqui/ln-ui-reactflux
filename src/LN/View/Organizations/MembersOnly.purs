module LN.View.Organizations.MembersOnly (
  renderView_Organizations_MembersOnly
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



renderView_Organizations_MembersOnly :: State -> ComponentHTML Input
renderView_Organizations_MembersOnly st =

  case st.currentOrganization, getLoading l_currentOrganization st.loading of
       _, true              -> renderLoading
       Just org_pack, false -> renderView_Organizations_MembersOnly' org_pack
       _, _                 -> H.div_ [H.text "organization unavailable"]



renderView_Organizations_MembersOnly' :: OrganizationPackResponse -> ComponentHTML Input
renderView_Organizations_MembersOnly' org_pack =
  H.div [P.class_ B.containerFluid] [
    H.div [P.class_ B.pageHeader] [
      H.h1 [P.class_ B.textCenter] [ H.text organization.name ]
    ],
    H.div [] [
      orgMemberHTML
        org_pack
        (\_ -> renderView_Organizations_MembersOnly_Already' org_pack)
        (\_ -> renderView_Organizations_MembersOnly_Join' org_pack)
    ]
  ]
  where
  organization = org_pack ^. _OrganizationPackResponse .. organization_ ^. _OrganizationResponse
  org_pack'    = org_pack ^. _OrganizationPackResponse



renderView_Organizations_MembersOnly_Already' :: OrganizationPackResponse -> ComponentHTML Input
renderView_Organizations_MembersOnly_Already' org_pack =
  H.div_ [
    H.p_ [H.text "You are already a member."]
  ]
  where
  organization = org_pack ^. _OrganizationPackResponse .. organization_ ^. _OrganizationResponse
  org_pack'    = org_pack ^. _OrganizationPackResponse



renderView_Organizations_MembersOnly_Join' :: OrganizationPackResponse -> ComponentHTML Input
renderView_Organizations_MembersOnly_Join' org_pack =
  H.div_ [
    H.p_ [
      H.text "This content is only available to members. Please ",
      linkToP [] (OrganizationsMembership organization.name Index emptyParams) "join",
      H.text " this organization in order to view this content."]
  ]
  where
  organization = org_pack ^. _OrganizationPackResponse .. organization_ ^. _OrganizationResponse
  org_pack'    = org_pack ^. _OrganizationPackResponse
