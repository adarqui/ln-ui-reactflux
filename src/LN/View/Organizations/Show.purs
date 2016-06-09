module LN.View.Organizations.Show (
  renderView_Organizations_Show
) where



import Daimyo.Data.ArrayList           (listToArray)
import Data.Map                        as M
import Data.Maybe                      (Maybe(..), maybe)
import Halogen                         (ComponentHTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3       as B
import Optic.Core                      ((^.), (..))
import Prelude                         (id, map, ($), (<>))

import LN.Input.Types                  (Input)
import LN.Router.Link                  (linkToP_Classes, linkToP_Glyph', linkToP)
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.State.Loading                (getLoading, l_currentOrganization)
import LN.State.Types                  (State)
import LN.View.Module.Loading          (renderLoading)
import LN.View.Forums.Show             (renderView_Forums_Show)
import LN.T                            ( OrganizationPackResponse
                                       , _OrganizationPackResponse, _OrganizationResponse, organization_
                                       , _ForumPackResponse, _ForumResponse, forum_)



renderView_Organizations_Show :: String -> State -> ComponentHTML Input
renderView_Organizations_Show org_name st =

  case st.currentOrganization, getLoading l_currentOrganization st.loading of
       _, true          -> renderLoading
       Nothing, false   -> H.div_ [H.text "organization unavailable"]
       Just pack, false -> renderView_Organizations_Show' pack st



renderView_Organizations_Show' :: OrganizationPackResponse -> State -> ComponentHTML Input
renderView_Organizations_Show' pack st =
  H.div [P.class_ B.containerFluid] [
    H.div [P.class_ B.pageHeader] [
      H.h1 [P.class_ B.textCenter] [ H.text organization.name ],
      H.p [P.class_ B.textCenter] [ H.text $ maybe "" id organization.description ],
      H.div_ [
        H.div_ [linkToP [] (Organizations (Edit organization.name) []) "edit"],
        H.div_ [linkToP [] (Organizations (Delete organization.name) []) "delete"]
      ]
    ],
    H.div [P.class_ B.container] [
      H.div [P.class_ B.pageHeader] [
        H.p_ [ H.h4_ [H.text "Name:", H.small_ [H.text $ " " <> organization.name]]],
        H.p_ [ H.h4_ [H.text "Company:", H.small_ [H.text $ " " <> organization.company]]],
        H.p_ [ H.h4_ [H.text "Location:", H.small_ [H.text $ " " <> organization.location]]]
      ],
      renderView_Forums_Show st,
      H.p_ [ H.h4_ [H.text "Members"]],
      H.p_ [ H.h4_ [H.text "teams"]],
      H.p_ [ H.h4_ [H.text "activity"]],
      H.p_ [ H.h4_ [H.text "stats"]]
    ]
  ]
  where
  organization = pack ^. _OrganizationPackResponse .. organization_ ^. _OrganizationResponse
