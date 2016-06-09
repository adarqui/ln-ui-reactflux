module LN.View.Threads.Mod (
  renderView_Threads_Delete,
  renderView_Threads_New,
  renderView_Threads_Edit,
  renderView_Threads_Mod,

  renderView_Threads_DeleteS,
  renderView_Threads_NewS,
  renderView_Threads_EditS
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
import LN.Input.Thread                 (Thread_Mod(..))
import LN.Input.Types                  (Input(..), cThreadMod)
import LN.Router.Class.Routes          (Routes(..))
import LN.State.Loading                (getLoading, l_currentThread)
import LN.State.Thread                 (ThreadRequestState)
import LN.State.Types                  (State)
import LN.View.Helpers                 (buttons_CreateEditCancel)
import LN.View.Module.Loading          (renderLoading)
import LN.T



renderView_Threads_Delete :: State -> ComponentHTML Input
renderView_Threads_Delete st =

  case st.currentThread, getLoading l_currentThread st.loading of
       _, true          -> renderLoading
       Nothing, false   -> H.div_ [H.p_ [H.text "thread unavailable."]]
       Just pack, false -> renderView_Threads_Delete' pack st



renderView_Threads_Delete' :: ThreadPackResponse -> State -> ComponentHTML Input
renderView_Threads_Delete' pack st =
  H.div_ [H.p_ [H.text "Delete? <yes/no>"]]
 where
 thread = pack ^. _ThreadPackResponse .. thread_ ^. _ThreadResponse



renderView_Threads_New :: State -> ComponentHTML Input
renderView_Threads_New = renderView_Threads_Mod Nothing



renderView_Threads_Edit :: Int -> State -> ComponentHTML Input
renderView_Threads_Edit thread_id = renderView_Threads_Mod (Just thread_id)



renderView_Threads_Mod :: Maybe Int -> State -> ComponentHTML Input
renderView_Threads_Mod m_thread_id st =
  case st.currentBoard, st.currentThreadRequest, st.currentThreadRequestSt, getLoading l_currentThread st.loading of
    _, _, _, true                                       -> renderLoading
    Just board_pack, Just thread_req, Just f_st, false  -> renderView_Threads_Mod' board_pack m_thread_id thread_req f_st st
    _, _, _, false                                      -> H.div_ [H.p_ [H.text "Threads_Mod: unexpected error."]]



renderView_Threads_Mod' :: BoardPackResponse -> Maybe Int -> ThreadRequest -> ThreadRequestState -> State -> ComponentHTML Input
renderView_Threads_Mod' board_pack m_thread_id thread_req f_st st =
  H.div_ [

    H.h1_ [ H.text "Add Thread" ]

  , input_Label "Name" "Name" thread.displayName P.InputText (E.input (cThreadMod <<< SetDisplayName))

  , textArea_Label "Description" "Description" (maybe "" id thread.description) (E.input (cThreadMod <<< SetDescription))

  -- , sticky

  -- , locked

  -- , poll

  -- , icon

  -- , tags

  , buttons_CreateEditCancel m_thread_id (cThreadMod $ Create board.id) (cThreadMod <<< EditP) About

  ]
  where
  thread    = unwrapThreadRequest thread_req
  board     = board_pack ^. _BoardPackResponse .. board_ ^. _BoardResponse



renderView_Threads_DeleteS :: State -> ComponentHTML Input
renderView_Threads_DeleteS = renderView_Threads_Delete



renderView_Threads_NewS :: State -> ComponentHTML Input
renderView_Threads_NewS = renderView_Threads_New



renderView_Threads_EditS :: State -> ComponentHTML Input
renderView_Threads_EditS st =

  case st.currentThread of

    Just thread_pack ->
      renderView_Threads_Edit
        (thread_pack ^. _ThreadPackResponse .. thread_ ^. _ThreadResponse .. id_)
        st

    _               -> H.div_ [H.text "error"]
