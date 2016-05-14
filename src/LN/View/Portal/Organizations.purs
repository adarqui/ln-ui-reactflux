module LN.View.Portal.Organizations (
  renderView_Portal_Organizations
) where



import Halogen                      (ComponentHTML)
import Optic.Core                   ((^.), (..))
import Prelude                      (map, ($))

import LN.Input.Types               (Input)
import LN.Router.Types              (Routes(..), CRUD(..))
import LN.State.Types               (State)
import LN.T
import LN.View.Module.Gravatar
import LN.View.Module.PageNumbers
import LN.View.Module.EntityListing



renderView_Portal_Organizations :: State -> ComponentHTML Input
renderView_Portal_Organizations st =
--  H.div_ $ map (\(OrganizationResponse org) -> H.p_ [H.text org.name]) st.organizations
  renderEntityListing "Organizations" (
    map (\(o@(OrganizationResponse org)) ->
      { nick: org.name
      , displayNick: org.name
      , createdAt: org.createdAt
      , logo: gravatarUrlFromOrganization XLarge o
      , route: Organizations (Show org.name)
      }
    ) st.organizations) pNum
  where
  pNum = renderPageNumbers st.organizationsPageInfo st.currentPage
