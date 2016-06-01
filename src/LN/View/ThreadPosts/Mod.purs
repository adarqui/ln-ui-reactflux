module LN.View.ThreadPosts.Mod (
  renderView_ThreadPosts_Delete,
  renderView_ThreadPosts_New,
  renderView_ThreadPosts_Edit,
  renderView_ThreadPosts_Mod
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
import LN.Input.ThreadPost                  (ThreadPost_Mod(..))
import LN.Input.Types                  (Input(..), cThreadPostMod)
import LN.State.Loading                (getLoading, l_currentThreadPost)
import LN.State.ThreadPost                  (ThreadPostRequestState)
import LN.State.Types                  (State)
import LN.View.Module.Loading          (renderLoading)
import LN.T



renderView_ThreadPosts_Delete :: Int -> Int -> State -> ComponentHTML Input
renderView_ThreadPosts_Delete organization_id threadPost_id st =

  case st.currentThreadPost, getLoading l_currentThreadPost st.loading of
       _, true          -> renderLoading
       Nothing, false   -> H.div_ [H.p_ [H.text "threadPost unavailable."]]
       Just pack, false -> renderView_ThreadPosts_Delete' pack st



renderView_ThreadPosts_Delete' :: ThreadPostPackResponse -> State -> ComponentHTML Input
renderView_ThreadPosts_Delete' pack st =
  H.div_ [H.p_ [H.text "Delete? <yes/no>"]]
 where
 threadPost = pack ^. _ThreadPostPackResponse .. threadPost_ ^. _ThreadPostResponse



renderView_ThreadPosts_New :: Int -> State -> ComponentHTML Input
renderView_ThreadPosts_New organization_id = renderView_ThreadPosts_Mod organization_id Nothing



renderView_ThreadPosts_Edit :: Int -> Int -> State -> ComponentHTML Input
renderView_ThreadPosts_Edit organization_id threadPost_id = renderView_ThreadPosts_Mod organization_id (Just threadPost_id)



renderView_ThreadPosts_Mod :: Int -> Maybe Int -> State -> ComponentHTML Input
renderView_ThreadPosts_Mod organization_id m_threadPost_id st =
  case st.currentThreadPostRequest, st.currentThreadPostRequestSt, getLoading l_currentThreadPost st.loading of
    _, _, true                         -> renderLoading
    Just threadPost_req, Just f_st, false   -> renderView_ThreadPosts_Mod' organization_id m_threadPost_id threadPost_req f_st st
    _, _, false                        -> H.div_ [H.p_ [H.text "ThreadPosts_Mod: unexpected error."]]



renderView_ThreadPosts_Mod' :: Int -> Maybe Int -> ThreadPostRequest -> ThreadPostRequestState -> State -> ComponentHTML Input
renderView_ThreadPosts_Mod' organization_id m_threadPost_id threadPost_req f_st st =
  H.div_ [

    H.h1_ [ H.text "Add ThreadPost" ]

  , create_or_save

  , create_or_save

  ]
  where
  threadPost    = unwrapThreadPostRequest threadPost_req
  save     = maybe "Create" (const "Save") m_threadPost_id
  create_or_save = case m_threadPost_id of
         Nothing         -> simpleInfoButton "Create" (cThreadPostMod $ Save organization_id)
         Just threadPost_id   -> simpleInfoButton "Save" (cThreadPostMod $ EditP threadPost_id)
         _               -> H.p_ [H.text "unexpected error."]
