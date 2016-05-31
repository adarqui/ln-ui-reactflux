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
import LN.State.Types                  (State)
import LN.View.Module.Loading          (renderLoading)
import LN.T                            ( OrganizationPackResponse
                                       , _OrganizationPackResponse, _OrganizationResponse, organization_
                                       , _ForumPackResponse, _ForumResponse, forum_)



renderView_Organizations_Show :: String -> State -> ComponentHTML Input
renderView_Organizations_Show org_name st =

  case st.currentOrganization of
       Nothing   -> renderLoading
       Just pack -> renderView_Organizations_Show' pack st



renderView_Organizations_Show' :: OrganizationPackResponse -> State -> ComponentHTML Input
renderView_Organizations_Show' pack st =
  H.div [P.class_ B.containerFluid] [
    H.div [P.class_ B.pageHeader] [
      H.h1 [P.class_ B.textCenter] [ H.text organization.name ],
      H.p [P.class_ B.textCenter] [ H.text $ maybe "" id organization.description ],
      H.div_ [
        H.div_ [linkToP [] (Organizations (EditI organization.id) []) "edit"],
        H.div_ [linkToP [] (Organizations (DeleteI organization.id) []) "delete"]
      ]
    ],
    H.div [P.class_ B.container] [
      H.div [P.class_ B.pageHeader] [
        H.p_ [ H.h4_ [H.text "Name:", H.small_ [H.text $ " " <> organization.name]]],
        H.p_ [ H.h4_ [H.text "Company:", H.small_ [H.text $ " " <> organization.company]]],
        H.p_ [ H.h4_ [H.text "Location:", H.small_ [H.text $ " " <> organization.location]]]
      ],
      forums organization.name st,
      H.p_ [ H.h4_ [H.text "Members"]],
      H.p_ [ H.h4_ [H.text "teams"]],
      H.p_ [ H.h4_ [H.text "activity"]],
      H.p_ [ H.h4_ [H.text "stats"]]
    ]
  ]
  where
  organization = pack ^. _OrganizationPackResponse .. organization_ ^. _OrganizationResponse



forums :: String -> State -> ComponentHTML Input
forums org_name st =
  H.div [P.class_ B.pageHeader] [

    H.h1 [P.class_ B.textCenter] [ H.text "Forums" ],

    H.div [P.classes [B.clearfix, B.container]] [
      H.div_ [linkToP [] (OrganizationsForums org_name New []) "add-forum"]
    ],

    H.div [P.class_ B.listUnstyled] $
      map (\forum_pack ->
        let forum = forum_pack ^. _ForumPackResponse .. forum_ ^. _ForumResponse in
        H.li_ [
          H.div [P.class_ B.row] [
            H.div [P.class_ B.colXs1] [
              H.p_ [H.text "icon"]
            ],
            H.div [P.class_ B.colXs6] [
              H.div [P.class_ B.listGroup] [linkToP_Classes [B.listGroupItem] [] (OrganizationsForums org_name (Show forum.name) []) forum.name],
              H.p_ [H.text $ maybe "No description." id forum.description]
            ],
            H.div [P.class_ B.colXs2] [
              H.p_ [H.text "boards"],
              H.p_ [H.text "threads"],
              H.p_ [H.text "posts"],
              H.p_ [H.text "views"]
            ],
            H.div [P.class_ B.colXs2] [
              H.p_ [H.text "created-at"]
            ],
            H.div [P.class_ B.colXs1] [
              H.div_ [linkToP [] (OrganizationsForums org_name (EditI forum.id) []) "edit"],
              H.div_ [linkToP [] (OrganizationsForums org_name (EditI forum.id) []) "delete"]
            ]
          ]
        ])
        $ listToArray $ M.values st.forums
  ]
