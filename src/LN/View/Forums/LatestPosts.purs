module LN.View.Forums.LatestPosts (
  renderView_Forums_LatestPosts,
  renderView_Forums_LatestPosts'
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

import LN.Input.Types                  (Input)
import LN.Router.Link                  (linkToP_Classes, linkToP_Glyph', linkToP)
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.Router.Class.Params          (emptyParams)
import LN.State.Types                  (State)
import LN.View.Helpers
import LN.View.Boards.Index            (renderView_Boards_Index')
import LN.View.Module.Loading          (renderLoading)
import LN.T                            ( ForumPackResponse
                                       , _ForumPackResponse, _ForumResponse, organization_, isOwner_
                                       , _ForumPackResponse, _ForumResponse, forum_
                                       , OrganizationPackResponse, OrganizationResponse
                                       , _OrganizationPackResponse, _OrganizationResponse
                                       , organization_)




renderView_Forums_LatestPosts :: State -> ComponentHTML Input
renderView_Forums_LatestPosts st =

  case st.currentOrganization, st.currentForum of

       Just org_pack, Just forum_pack ->
         renderView_Forums_LatestPosts' org_pack forum_pack -- st.latestPosts

       _, _                           -> renderLoading



--
-- Re: ADARQ's Journal by adarqui (Progress Journals & Experimental Routines) Today at 06:00:30 pm
--
renderView_Forums_LatestPosts'
  :: OrganizationPackResponse
  -> ForumPackResponse
  -> ComponentHTML Input
renderView_Forums_LatestPosts' org_pack forum_pack =
  H.div [P.class_ B.containerFluid] [

    H.div [P.class_ B.pageHeader] [
      H.h4_ [H.text "Recent Posts"]
    ]
  ]
  where
  org        = org_pack ^. _OrganizationPackResponse .. organization_ ^. _OrganizationResponse
  org_owner  = org_pack ^. _OrganizationPackResponse .. isOwner_
  forum      = forum_pack ^. _ForumPackResponse .. forum_ ^. _ForumResponse
