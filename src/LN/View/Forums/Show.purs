module LN.View.Forums.Show (
  renderView_Forums_Show,
  renderView_Forums_Show'
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
import LN.View.Module.Loading          (renderLoading)
import LN.T                            ( ForumPackResponse
                                       , _ForumPackResponse, _ForumResponse, organization_
                                       , _ForumPackResponse, _ForumResponse, forum_
                                       , OrganizationPackResponse, OrganizationResponse
                                       , _OrganizationPackResponse, _OrganizationResponse
                                       , organization_)




renderView_Forums_Show :: State -> ComponentHTML Input
renderView_Forums_Show st =

  case st.currentOrganization  of
       Just org_pack -> renderView_Forums_Show' org_pack st
       _  -> renderLoading



renderView_Forums_Show' :: OrganizationPackResponse -> State -> ComponentHTML Input
renderView_Forums_Show' org_pack st =
  H.div [P.class_ B.pageHeader] [

    H.h1 [P.class_ B.textCenter] [ H.text "Forums" ],

    H.div [P.classes [B.clearfix, B.container]] [
      H.div_ [linkToP [] (OrganizationsForums org.name New []) "add-forum"]
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
              H.div [P.class_ B.listGroup] [linkToP_Classes [B.listGroupItem] [] (OrganizationsForums org.name (Show forum.name) []) forum.name],
              H.p_ [H.text $ maybe "No description." id forum.description],
              if forum.tags /= []
                 then H.p_ [H.text $ show forum.tags]
                 else H.div_ []
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
              H.div_ [linkToP [] (OrganizationsForums org.name (Edit $ show forum.id) []) "edit"],
              H.div_ [linkToP [] (OrganizationsForums org.name (Delete $ show forum.id) []) "delete"]
            ]
          ]
        ])
        $ listToArray $ M.values st.forums
  ]
  where
  org = org_pack ^. _OrganizationPackResponse .. organization_ ^. _OrganizationResponse
