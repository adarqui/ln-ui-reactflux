module LN.View.Forums.Mod (
  renderView_Forums_Delete,
  renderView_Forums_New,
  renderView_Forums_Edit,
  renderView_Forums_Mod
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
import LN.State.Loading                (getLoading, l_currentForum)
import LN.State.Forum                  (ForumRequestState)
import LN.State.Types                  (State)
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

  , create_or_save

  , input_Label "Name" "Name" forum.name P.InputText (E.input (cForumMod <<< SetName))

  , textArea_Label "Description" "Description" (maybe "" id forum.description) (E.input (cForumMod <<< SetDescription))

  , create_or_save

  ]
  where
  forum    = unwrapForumRequest forum_req
  save     = maybe "Create" (const "Save") m_forum_id
  create_or_save = case m_forum_id of
         Nothing         -> simpleInfoButton "Create" (cForumMod $ Save organization_id)
         Just forum_id   -> simpleInfoButton "Save" (cForumMod $ EditP forum_id)
         _               -> H.p_ [H.text "unexpected error."]
