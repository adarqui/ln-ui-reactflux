module LN.View.Organizations.Show (
  renderView_Organizations_Show
) where



import Data.Maybe                      (Maybe(..), maybe)
import Halogen                         (ComponentHTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3       as B
import Optic.Core                      ((^.), (..))
import Prelude                         (id, map, ($), (<>))

import LN.Input.Types                  (Input)
import LN.Router.Internal              (linkToP)
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.State.Types                  (State)
import LN.State.Lens
import LN.T



renderView_Organizations_Show :: String -> State -> ComponentHTML Input
renderView_Organizations_Show org_name st =
  H.div [P.class_ B.containerFluid] [
    H.div [P.class_ B.pageHeader] [
      H.h1 [P.class_ B.textCenter] [ H.text (org_name) ],
      H.p [P.class_ B.textCenter] [ H.text $ maybe "" id desc ]
    ],
    H.div [P.class_ B.container] [
      H.div [P.class_ B.pageHeader] [
        H.p_ [ H.h4_ [H.text "Name:", H.small_ [H.text $ " " <> name]]],
        H.p_ [ H.h4_ [H.text "Company:", H.small_ [H.text $ " " <> company]]],
        H.p_ [ H.h4_ [H.text "Location:", H.small_ [H.text $ " " <> location]]]
      ],
      forums name st,
      H.p_ [ H.h4_ [H.text "Members"]],
      H.p_ [ H.h4_ [H.text "teams"]],
      H.p_ [ H.h4_ [H.text "activity"]],
      H.p_ [ H.h4_ [H.text "stats"]]
    ]
  ]
  where
  name = maybe "Empty" (\org -> org ^. _OrganizationResponse .. name_) (st ^. stCurrentOrganization)
  desc = maybe Nothing (\org -> org ^. _OrganizationResponse .. description_) (st ^. stCurrentOrganization)
  company = maybe "Empty" (\org -> org ^. _OrganizationResponse .. company_) (st ^. stCurrentOrganization)
  location = maybe "Empty" (\org -> org ^. _OrganizationResponse .. location_) (st ^. stCurrentOrganization)



forums :: String -> State -> ComponentHTML Input
forums name st =
  H.div [P.class_ B.pageHeader] [
    H.h1 [P.class_ B.textCenter] [ H.text "Forums" ],
    H.div [P.class_ B.listUnstyled] $
      map (\(ForumResponse forum) ->
        H.li_ [
          H.div [P.class_ B.row] [
            H.div [P.class_ B.colSm1] [
              H.p_ [H.text "icon"]
            ],
            H.div [P.class_ B.colSm6] [
              linkToP [] (OrganizationsForums name (Show forum.name) []) forum.name,
              H.p_ [H.text $ maybe "No description." id forum.description]
            ],
            H.div [P.class_ B.colSm2] [
              H.p_ [H.text "boards"],
              H.p_ [H.text "threads"],
              H.p_ [H.text "posts"],
              H.p_ [H.text "views"]
            ],
            H.div [P.class_ B.colSm3] [
              H.p_ [H.text "created-at"]
            ]
          ]
        ])
        st.forums
  ]
