module LN.View.Forums.Index (
  renderView_Forums_Index,
  renderView_Forums_Index'
) where



import Daimyo.Data.ArrayList           (listToArray)
import Data.Map                        as M
import Data.Maybe                      (Maybe(..), maybe)
import Halogen                         (ComponentHTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3       as B
import Optic.Core                      ((^.), (..))
import Prelude                         (id, map, show, ($), (<>), (/=))

import LN.Input.Types                  (Input)
import LN.Router.Link                  (linkToP_Classes, linkToP_Glyph', linkToP)
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.State.Types                  (State)
import LN.View.Helpers
import LN.View.Module.Loading          (renderLoading)
import LN.T                            ( ForumPackResponse
                                       , _ForumPackResponse, _ForumResponse, organization_
                                       , _ForumPackResponse, _ForumResponse, forum_
                                       , OrganizationPackResponse, OrganizationResponse
                                       , _OrganizationPackResponse, _OrganizationResponse
                                       , organization_)




renderView_Forums_Index :: State -> ComponentHTML Input
renderView_Forums_Index st =

  case st.currentOrganization of
       Just org_pack -> renderView_Forums_Index' org_pack st.forums
       _             -> renderLoading



renderView_Forums_Index' :: OrganizationPackResponse -> M.Map Int ForumPackResponse -> ComponentHTML Input
renderView_Forums_Index' org_pack forum_packs =
  H.div [P.class_ B.pageHeader] [

    H.h1 [P.class_ B.textCenter] [ H.text "Forums" ],

    H.div [P.classes [B.clearfix, B.container]] [
      glyphButtonLinkDef_Plus $ OrganizationsForums org.name New []
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
              H.div [P.class_ B.listGroup] [linkToP_Classes [B.listGroupItem] [] (OrganizationsForums org.name (Show forum.name) []) forum.displayName],
              H.p_ [H.text $ maybe "No description." id forum.description],
              showTagsSmall forum.tags
--              if forum.tags /= []
--                 then H.p_ [H.text $ show forum.tags]
--                 else H.div_ []
            ],
            H.div [P.class_ B.colXs2] [
              showBadge' "boards "  0,
              showBadge' "threads " 0,
              showBadge' "posts "   0,
              showBadge' "views "   0
            ],
            H.div [P.class_ B.colXs2] [
              H.p_ [H.text "created-at"]
            ],
            H.div [P.class_ B.colXs1] [
              buttonGroup_Horizontal [
                glyphButtonLinkDef_Pencil $ OrganizationsForums org.name (Edit forum.name) [],
                glyphButtonLinkDef_Trash $ OrganizationsForums org.name (Delete forum.name) []
              ]
            ]
          ]
        ])
        $ listToArray $ M.values forum_packs
  ]
  where
  org = org_pack ^. _OrganizationPackResponse .. organization_ ^. _OrganizationResponse
