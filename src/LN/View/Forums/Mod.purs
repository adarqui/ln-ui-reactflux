module LN.View.Forums.Mod (
  renderView_Forums_Delete,
  renderView_Forums_New,
  renderView_Forums_Edit,
  renderView_Forums_Mod,

  renderView_Forums_DeleteS,
  renderView_Forums_NewS,
  renderView_Forums_EditS
) where



import Data.Maybe                      (Maybe(..), maybe)
import Data.Tuple                      (Tuple(..))
import Halogen                         (ComponentHTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Events             as E
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3       as B
import Optic.Core                      ((^.), (..))
import Prelude                         (id, map, show, const, ($), (<<<), (<>))

import LN.Halogen.Util
import LN.Helpers.Array                (seqArrayFrom)
import LN.Helpers.JSON                 (decodeString)
import LN.Internal.Forum               ()
import LN.Input.Forum                  (Forum_Mod(..))
import LN.Input.Types                  (Input(..), cForumMod)
import LN.Router.Class.CRUD            (TyCRUD(..))
import LN.Router.Class.Routes          (Routes(..))
import LN.State.Loading                (getLoading, l_currentForum)
import LN.State.Forum                  (ForumRequestState)
import LN.State.Types                  (State)
import LN.View.Fields                  ( mandatoryNameField, optionalDescriptionField, mandatoryIntegerField, mandatoryVisibilityField
                                       , tagsField)
import LN.View.Helpers                 (buttons_CreateEditCancel)
import LN.View.Module.Loading          (renderLoading)
import LN.T



renderView_Forums_Delete :: Int -> Int -> State -> ComponentHTML Input
renderView_Forums_Delete organization_id forum_id st =

  case st.currentForum, getLoading l_currentForum st.loading of
       _, true          -> renderLoading
       Nothing, false   -> H.div_ [H.p_ [H.text "forum unavailable."]]
       Just pack, false -> renderView_Forums_Delete' pack st



renderView_Forums_Delete' :: ForumPackResponse -> State -> ComponentHTML Input
renderView_Forums_Delete' pack st =
  H.div_ [H.p_ [H.text "Delete? <yes/no>"]]
 where
 forum = pack ^. _ForumPackResponse .. forum_ ^. _ForumResponse



renderView_Forums_New :: Int -> State -> ComponentHTML Input
renderView_Forums_New organization_id = renderView_Forums_Mod TyCreate organization_id Nothing



renderView_Forums_Edit :: Int -> Int -> State -> ComponentHTML Input
renderView_Forums_Edit organization_id forum_id = renderView_Forums_Mod TyEdit organization_id (Just forum_id)



renderView_Forums_Mod :: TyCRUD -> Int -> Maybe Int -> State -> ComponentHTML Input
renderView_Forums_Mod crud organization_id m_forum_id st =
  case st.currentForumRequest, st.currentForumRequestSt, getLoading l_currentForum st.loading of
    _, _, true                               -> renderLoading
    Just forum_req, Just forum_req_st, false -> renderView_Forums_Mod' crud organization_id m_forum_id forum_req forum_req_st
    _, _, false                              -> H.div_ [H.p_ [H.text "Forums_Mod: unexpected error."]]



renderView_Forums_Mod' :: TyCRUD -> Int -> Maybe Int -> ForumRequest -> ForumRequestState -> ComponentHTML Input
renderView_Forums_Mod' crud organization_id m_forum_id forum_req forum_req_st =
  H.div_ [

    H.h1_ [ H.text $ show crud <> " Forum" ]

  , mandatoryNameField forum.displayName (cForumMod <<< SetDisplayName)

  , optionalDescriptionField forum.description (cForumMod <<< SetDescription) (cForumMod RemoveDescription)

  , mandatoryIntegerField "Threads per board" forum.threadsPerBoard 20 10 50 10 (cForumMod <<< SetThreadsPerBoard)

  , mandatoryIntegerField "Posts per thread" forum.threadPostsPerThread 20 10 50 10 (cForumMod <<< SetThreadPostsPerThread)

  , mandatoryIntegerField "Recent threads (limit)" forum.recentThreadsLimit 10 0 20 1 (cForumMod <<< SetRecentThreadsLimit)

  , mandatoryIntegerField "Recent posts (limit)" forum.recentPostsLimit 10 0 20 1 (cForumMod <<< SetRecentPostsLimit)

  , mandatoryIntegerField "Messages of the week (limit)" forum.motwLimit 10 0 20 1 (cForumMod <<< SetMotwLimit)

  , mandatoryVisibilityField forum.visibility (cForumMod <<< SetVisibility)

  , tagsField
      forum.tags
      (maybe "" id forum_req_st.currentTag)
      (cForumMod <<< SetTag)
      (cForumMod AddTag)
      (cForumMod <<< DeleteTag)
      (cForumMod ClearTags)

  , buttons_CreateEditCancel m_forum_id (cForumMod $ Create organization_id) (cForumMod <<< EditP) About

  ]
  where
  forum    = unwrapForumRequest forum_req



renderView_Forums_DeleteS :: State -> ComponentHTML Input
renderView_Forums_DeleteS st =

  case st.currentOrganization, st.currentForum of

    Just org_pack, Just forum_pack ->
      renderView_Forums_Delete
        (org_pack ^. _OrganizationPackResponse .. organization_ ^. _OrganizationResponse .. id_)
        (forum_pack ^. _ForumPackResponse .. forum_ ^. _ForumResponse .. id_)
        st

    _, _       -> H.div_ [H.text "error"]



renderView_Forums_NewS :: State -> ComponentHTML Input
renderView_Forums_NewS st =
  case st.currentOrganization of
    Nothing       -> H.div_ [H.text "error"]
    Just org_pack -> renderView_Forums_New (org_pack ^. _OrganizationPackResponse .. organization_ ^. _OrganizationResponse .. id_) st



renderView_Forums_EditS :: State -> ComponentHTML Input
renderView_Forums_EditS st =

  case st.currentOrganization, st.currentForum of

    Just org_pack, Just forum_pack ->
      renderView_Forums_Edit
        (org_pack ^. _OrganizationPackResponse .. organization_ ^. _OrganizationResponse .. id_)
        (forum_pack ^. _ForumPackResponse .. forum_ ^. _ForumResponse .. id_)
        st

    _, _       -> H.div_ [H.text "error"]
