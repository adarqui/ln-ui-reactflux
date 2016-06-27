module LN.View.Forums.Show (
  renderView_Forums_Show,
  renderView_Forums_Show'
) where



import LN.ArrayList                    (listToArray)
import Data.Map                        as M
import Data.Maybe                      (Maybe(..), maybe)
import Halogen                         (ComponentHTML, HTML)
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
import LN.View.Boards.Index            (renderView_Boards_Index')
import LN.View.Forums.RecentPosts      (renderView_Forums_RecentPosts')
import LN.View.Forums.MessagesOfTheWeek(renderView_Forums_MessagesOfTheWeek')
import LN.View.Module.Loading          (renderLoading)
import LN.T                            ( ForumPackResponse
                                       , _ForumPackResponse, _ForumResponse, organization_
                                       , _ForumPackResponse, _ForumResponse, forum_
                                       , OrganizationPackResponse, OrganizationResponse
                                       , _OrganizationPackResponse, _OrganizationResponse
                                       , organization_
                                       , ThreadPostPackResponse)




renderView_Forums_Show :: State -> ComponentHTML Input
renderView_Forums_Show st =

  case st.currentOrganization, st.currentForum of

       Just org_pack, Just forum_pack ->
         renderView_Forums_Show' org_pack forum_pack
           (renderView_Boards_Index' org_pack forum_pack st.boards)
           (renderView_Forums_RecentPosts' org_pack forum_pack st.recentThreadPosts)
           (renderView_Forums_MessagesOfTheWeek' org_pack forum_pack)

       _, _                           -> renderLoading



renderView_Forums_Show'
  :: OrganizationPackResponse
  -> ForumPackResponse
  -> HTML _ _
  -> HTML _ _
  -> HTML _ _
  -> ComponentHTML Input
renderView_Forums_Show'
  org_pack
  forum_pack
  plumbing_boards
  plumbing_recent_posts
  plumbing_messages_of_the_week
  =
  H.div [P.class_ B.containerFluid] [

    H.div [P.class_ B.pageHeader] [
      H.h2_ [H.text forum.name],
      H.p [P.class_ B.lead] [H.text forum_desc],

        -- ACCESS: Forum
        -- * Create: can create boards within a forum
        -- * Update: can edit forum settings
        -- * Delete: can delete the forum
        --
        buttonGroup_HorizontalSm1 [
          permissionsHTML'
            forum_pack'.permissions
            (\_ -> button_newBoard $ OrganizationsForumsBoards org.name forum.name New emptyParams)
            permReadEmpty
            (\_ -> button_editForum $ OrganizationsForums org.name (Edit forum.name) emptyParams)
            (\_ -> button_deleteForum $ OrganizationsForums org.name (Delete forum.name) emptyParams)
            permExecuteEmpty
        ]

    ],

    H.div [] [plumbing_boards],

    H.div [] [plumbing_recent_posts],

    H.div [] [plumbing_messages_of_the_week]

  ]
  where
  org         = org_pack ^. _OrganizationPackResponse .. organization_ ^. _OrganizationResponse
  forum       = forum_pack ^. _ForumPackResponse .. forum_ ^. _ForumResponse
  forum_pack' = forum_pack ^. _ForumPackResponse
  forum_desc  = maybe "No description." id forum.description
