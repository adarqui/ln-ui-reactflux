module LN.View.Forums.Index (
  renderView_Forums_Index,
  renderView_Forums_Index'
) where



import LN.ArrayList                    (listToArray)
import Data.Map                        as M
import Data.Maybe                      (Maybe(..), maybe)
import Halogen                         (ComponentHTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3       as B
import Optic.Core                      ((^.), (..))
import Prelude                         (id, map, show, ($), (<>), (/=))

import LN.Access
import LN.Input.Types                  (Input)
import LN.Router.Link                  (linkToP_Classes, linkToP_Glyph', linkToP)
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.Router.Class.Params          (emptyParams)
import LN.State.Types                  (State)
import LN.View.Helpers
import LN.View.Module.Loading          (renderLoading)
import LN.T                            ( ForumPackResponse
                                       , _ForumPackResponse, _ForumResponse, organization_
                                       , _ForumPackResponse, _ForumResponse, forum_
                                       , _ForumStatResponse
                                       , OrganizationPackResponse, OrganizationResponse
                                       , _OrganizationPackResponse, _OrganizationResponse
                                       , organization_
                                       , stat_)




renderView_Forums_Index :: State -> ComponentHTML Input
renderView_Forums_Index st =

  case st.currentOrganization of
       Just org_pack -> renderView_Forums_Index' org_pack st.forums
       _             -> renderLoading



renderView_Forums_Index' :: OrganizationPackResponse -> M.Map Int ForumPackResponse -> ComponentHTML Input
renderView_Forums_Index' org_pack forum_packs =
  H.div [P.class_ B.pageHeader] [

    H.h1 [P.class_ B.textCenter] [ H.text "Forums" ],

    -- ACCESS: Organization
    -- * Create: can create forums
    --
    permissionsMatchCreateHTML
      org_pack'.permissions
      (\_ -> button_newForum $ OrganizationsForums org.name New emptyParams)
      unitDiv,

    H.div [P.class_ B.listUnstyled] $
      map (\forum_pack ->
        let
          forum       = forum_pack ^. _ForumPackResponse .. forum_ ^. _ForumResponse
          forum_pack' = forum_pack ^. _ForumPackResponse
          stat        = forum_pack ^. _ForumPackResponse .. stat_ ^. _ForumStatResponse
        in
        H.li_ [
          H.div [P.class_ B.row] [
            H.div [P.class_ B.colXs1] [
              H.p_ [H.text "icon"]
            ],
            H.div [P.class_ B.colXs6] [
              H.div [P.class_ B.listGroup] [linkToP_Classes [B.listGroupItem] [] (OrganizationsForums org.name (Show forum.name) emptyParams) forum.displayName],
              H.p_ [H.text $ maybe "No description." id forum.description],
              showTagsSmall forum.tags
            ],
            H.div [P.class_ B.colXs2] [
              showBadge' "boards "  stat.boards,
              showBadge' "threads " stat.threads,
              showBadge' "posts "   stat.threadPosts,
              showBadge' "views "   stat.views
            ],
            H.div [P.class_ B.colXs2] [
              H.p_ [H.text "created-at"]
            ],
            H.div [P.class_ B.colXs1] [

              -- ACCESS: Forum
              -- * Update: can edit forum settings
              -- * Delete: can delete a forum
              --
              permissionsHTML'
                forum_pack'.permissions
                permCreateEmpty
                permReadEmpty
                (\_ -> button_editForum $ OrganizationsForums org.name (Edit forum.name) emptyParams)
                (\_ -> button_deleteForum $ OrganizationsForums org.name (Delete forum.name) emptyParams)
                permExecuteEmpty
            ]
          ]
        ])
        $ listToArray $ M.values forum_packs
  ]
  where
  org       = org_pack ^. _OrganizationPackResponse .. organization_ ^. _OrganizationResponse
  org_pack' = org_pack ^. _OrganizationPackResponse
--  col_stats = if org_owner then B.colXs2 else B.colXs3
