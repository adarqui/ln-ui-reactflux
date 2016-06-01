module LN.View.Threads.Mod (
  renderView_Threads_Delete,
  renderView_Threads_New,
  renderView_Threads_Edit,
  renderView_Threads_Mod
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
import LN.Input.Thread                  (Thread_Mod(..))
import LN.Input.Types                  (Input(..), cThreadMod)
import LN.State.Loading                (getLoading, l_currentThread)
import LN.State.Thread                  (ThreadRequestState)
import LN.State.Types                  (State)
import LN.View.Module.Loading          (renderLoading)
import LN.T



renderView_Threads_Delete :: Int -> Int -> State -> ComponentHTML Input
renderView_Threads_Delete organization_id thread_id st =

  case st.currentThread, getLoading l_currentThread st.loading of
       _, true          -> renderLoading
       Nothing, false   -> H.div_ [H.p_ [H.text "thread unavailable."]]
       Just pack, false -> renderView_Threads_Delete' pack st



renderView_Threads_Delete' :: ThreadPackResponse -> State -> ComponentHTML Input
renderView_Threads_Delete' pack st =
  H.div_ [H.p_ [H.text "Delete? <yes/no>"]]
 where
 thread = pack ^. _ThreadPackResponse .. thread_ ^. _ThreadResponse



renderView_Threads_New :: Int -> State -> ComponentHTML Input
renderView_Threads_New organization_id = renderView_Threads_Mod organization_id Nothing



renderView_Threads_Edit :: Int -> Int -> State -> ComponentHTML Input
renderView_Threads_Edit organization_id thread_id = renderView_Threads_Mod organization_id (Just thread_id)



renderView_Threads_Mod :: Int -> Maybe Int -> State -> ComponentHTML Input
renderView_Threads_Mod organization_id m_thread_id st =
  case st.currentThreadRequest, st.currentThreadRequestSt, getLoading l_currentThread st.loading of
    _, _, true                         -> renderLoading
    Just thread_req, Just f_st, false   -> renderView_Threads_Mod' organization_id m_thread_id thread_req f_st st
    _, _, false                        -> H.div_ [H.p_ [H.text "Threads_Mod: unexpected error."]]



renderView_Threads_Mod' :: Int -> Maybe Int -> ThreadRequest -> ThreadRequestState -> State -> ComponentHTML Input
renderView_Threads_Mod' organization_id m_thread_id thread_req f_st st =
  H.div_ [

    H.h1_ [ H.text "Add Thread" ]

  , create_or_save

  , create_or_save

  ]
  where
  thread    = unwrapThreadRequest thread_req
  save     = maybe "Create" (const "Save") m_thread_id
  create_or_save = case m_thread_id of
         Nothing         -> simpleInfoButton "Create" (cThreadMod $ Save organization_id)
         Just thread_id   -> simpleInfoButton "Save" (cThreadMod $ EditP thread_id)
         _               -> H.p_ [H.text "unexpected error."]
