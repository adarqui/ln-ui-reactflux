module LN.View.Boards.Mod (
  renderView_Boards_Delete,
  renderView_Boards_New,
  renderView_Boards_Edit,
  renderView_Boards_Mod,

  renderView_Boards_NewS,
  renderView_Boards_EditS,
  renderView_Boards_DeleteS
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
import LN.Input.Board                  (Board_Mod(..))
import LN.Input.Types                  (Input(..), cBoardMod)
import LN.Router.Class.Routes          (Routes(..))
import LN.State.Loading                (getLoading, l_currentBoard)
import LN.State.Board                  (BoardRequestState)
import LN.State.Types                  (State)
import LN.View.Helpers                 (buttons_CreateEditCancel)
import LN.View.Module.Loading          (renderLoading)
import LN.T



renderView_Boards_Delete :: State -> ComponentHTML Input
renderView_Boards_Delete st =

  case st.currentBoard, getLoading l_currentBoard st.loading of
       _, true          -> renderLoading
       Nothing, false   -> H.div_ [H.p_ [H.text "board unavailable."]]
       Just pack, false -> renderView_Boards_Delete' pack st



renderView_Boards_Delete' :: BoardPackResponse -> State -> ComponentHTML Input
renderView_Boards_Delete' pack st =
  H.div_ [H.p_ [H.text "Delete? <yes/no>"]]
 where
 board = pack ^. _BoardPackResponse .. board_ ^. _BoardResponse



renderView_Boards_New :: State -> ComponentHTML Input
renderView_Boards_New = renderView_Boards_Mod Nothing



renderView_Boards_Edit :: Int -> State -> ComponentHTML Input
renderView_Boards_Edit board_id = renderView_Boards_Mod (Just board_id)



renderView_Boards_Mod :: Maybe Int -> State -> ComponentHTML Input
renderView_Boards_Mod m_board_id st =
  case st.currentForum, st.currentBoardRequest, st.currentBoardRequestSt, getLoading l_currentBoard st.loading of
    _, _, _, true                                     -> renderLoading
    Just forum_pack, Just board_req, Just f_st, false -> renderView_Boards_Mod' forum_pack m_board_id board_req f_st st
    _, _, _, false                                    -> H.div_ [H.p_ [H.text "Boards_Mod: unexpected error."]]



renderView_Boards_Mod' :: ForumPackResponse -> Maybe Int -> BoardRequest -> BoardRequestState -> State -> ComponentHTML Input
renderView_Boards_Mod' forum_pack m_board_id board_req f_st st =
  H.div_ [

    H.h1_ [ H.text "Add Board" ]

  , input_Label "Name" "Name" board.displayName P.InputText (E.input (cBoardMod <<< SetDisplayName))

  , textArea_Label "Description" "Description" (maybe "" id board.description) (E.input (cBoardMod <<< SetDescription))

  , buttons_CreateEditCancel m_board_id (cBoardMod $ Create forum.id) (cBoardMod <<< EditP) About

  ]
  where
  board    = unwrapBoardRequest board_req
  forum    = forum_pack ^. _ForumPackResponse .. forum_ ^. _ForumResponse



renderView_Boards_DeleteS :: State -> ComponentHTML Input
renderView_Boards_DeleteS = renderView_Boards_Delete



renderView_Boards_NewS :: State -> ComponentHTML Input
renderView_Boards_NewS = renderView_Boards_New



renderView_Boards_EditS :: State -> ComponentHTML Input
renderView_Boards_EditS st =

  case st.currentOrganization, st.currentBoard of

    Just org_pack, Just board_pack ->
      renderView_Boards_Edit
        (board_pack ^. _BoardPackResponse .. board_ ^. _BoardResponse .. id_)
        st

    _, _       -> H.div_ [H.text "error"]
