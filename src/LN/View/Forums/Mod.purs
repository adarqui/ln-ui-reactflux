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
import Prelude                         (id, map, show, const, ($), (<<<))

import LN.Halogen.Util
import LN.Helpers.Array                (seqArrayFrom)
import LN.Helpers.JSON                 (decodeString)
import LN.Internal.Forum               ()
import LN.Input.Forum                  (Forum_Mod(..))
import LN.Input.Types                  (Input(..), cForumMod)
import LN.Router.Class.Routes          (Routes(..))
import LN.State.Loading                (getLoading, l_currentForum)
import LN.State.Forum                  (ForumRequestState)
import LN.State.Types                  (State)
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
renderView_Forums_New organization_id = renderView_Forums_Mod organization_id Nothing



renderView_Forums_Edit :: Int -> Int -> State -> ComponentHTML Input
renderView_Forums_Edit organization_id forum_id = renderView_Forums_Mod organization_id (Just forum_id)



renderView_Forums_Mod :: Int -> Maybe Int -> State -> ComponentHTML Input
renderView_Forums_Mod organization_id m_forum_id st =
  case st.currentForumRequest, st.currentForumRequestSt, getLoading l_currentForum st.loading of
    _, _, true                         -> renderLoading
    Just forum_req, Just f_st, false   -> renderView_Forums_Mod' organization_id m_forum_id forum_req f_st st
    _, _, false                        -> H.div_ [H.p_ [H.text "Forums_Mod: unexpected error."]]



renderView_Forums_Mod' :: Int -> Maybe Int -> ForumRequest -> ForumRequestState -> State -> ComponentHTML Input
renderView_Forums_Mod' organization_id m_forum_id forum_req f_st st =
  H.div_ [

    H.h1_ [ H.text "Add Forum" ]

  , input_Label "Name" "Name" forum.displayName P.InputText (E.input (cForumMod <<< SetDisplayName))

  , textArea_Label "Description" "Description" (maybe "" id forum.description) (E.input (cForumMod <<< SetDescription))

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
