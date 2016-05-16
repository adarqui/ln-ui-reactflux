module LN.View.Organizations.Index (
  renderView_Organizations_Index
) where



import Daimyo.Data.ArrayList        (listToArray)
import Data.Map                     as M
import Halogen                      (ComponentHTML)
import Optic.Core                   ((^.), (..))
import Prelude                      (map, ($))

import LN.Input.Types               (Input)
import LN.Router.Types              (Routes(..), CRUD(..))
import LN.State.Types               (State)
import LN.T                         (Size(XLarge), organization_, _OrganizationPackResponse
                                    , _OrganizationResponse)
import LN.View.Module.Gravatar      (gravatarUrlFromOrganization)
import LN.View.Module.PageNumbers   (renderPageNumbers)
import LN.View.Module.EntityListing (renderEntityListing)



renderView_Organizations_Index :: State -> ComponentHTML Input
renderView_Organizations_Index st =
  renderEntityListing "Organizations" (
    map (\pack ->
      let org = pack ^. _OrganizationPackResponse .. organization_ ^. _OrganizationResponse
      in
      { nick:        org.name
      , displayNick: org.name
      , createdAt:   org.createdAt
      , logo:        gravatarUrlFromOrganization XLarge (pack ^. _OrganizationPackResponse .. organization_)
      , route:       Organizations (Show org.name) []
      }
    ) $ listToArray $ M.values st.organizations) pNum
  where
  pNum = renderPageNumbers st.organizationsPageInfo st.currentPage
