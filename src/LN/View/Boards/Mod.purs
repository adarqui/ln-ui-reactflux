module LN.View.Boards.Mod (
  renderView_Boards_Delete,
  renderView_Boards_New,
  renderView_Boards_Edit,
  renderView_Boards_Mod
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
import LN.State.Loading                (getLoading, l_currentBoard)
import LN.State.Board                  (BoardRequestState)
import LN.State.Types                  (State)
import LN.View.Module.Loading          (renderLoading)
import LN.T



renderView_Boards_Delete :: Int -> Int -> State -> ComponentHTML Input
renderView_Boards_Delete organization_id board_id st =

  case st.currentBoard, getLoading l_currentBoard st.loading of
       _, true          -> renderLoading
       Nothing, false   -> H.div_ [H.p_ [H.text "board unavailable."]]
       Just pack, false -> renderView_Boards_Delete' pack st



renderView_Boards_Delete' :: BoardPackResponse -> State -> ComponentHTML Input
renderView_Boards_Delete' pack st =
  H.div_ [H.p_ [H.text "Delete? <yes/no>"]]
 where
 board = pack ^. _BoardPackResponse .. board_ ^. _BoardResponse



renderView_Boards_New :: Int -> State -> ComponentHTML Input
renderView_Boards_New organization_id = renderView_Boards_Mod organization_id Nothing



renderView_Boards_Edit :: Int -> Int -> State -> ComponentHTML Input
renderView_Boards_Edit organization_id board_id = renderView_Boards_Mod organization_id (Just board_id)



renderView_Boards_Mod :: Int -> Maybe Int -> State -> ComponentHTML Input
renderView_Boards_Mod organization_id m_board_id st =
  case st.currentBoardRequest, st.currentBoardRequestSt, getLoading l_currentBoard st.loading of
    _, _, true                         -> renderLoading
    Just board_req, Just f_st, false   -> renderView_Boards_Mod' organization_id m_board_id board_req f_st st
    _, _, false                        -> H.div_ [H.p_ [H.text "Boards_Mod: unexpected error."]]



renderView_Boards_Mod' :: Int -> Maybe Int -> BoardRequest -> BoardRequestState -> State -> ComponentHTML Input
renderView_Boards_Mod' organization_id m_board_id board_req f_st st =
  H.div_ [

    H.h1_ [ H.text "Add Board" ]

  , create_or_save

  , create_or_save

  ]
  where
  board    = unwrapBoardRequest board_req
  save     = maybe "Create" (const "Save") m_board_id
  create_or_save = case m_board_id of
         Nothing         -> simpleInfoButton "Create" (cBoardMod $ Save organization_id)
         Just board_id   -> simpleInfoButton "Save" (cBoardMod $ EditP board_id)
         _               -> H.p_ [H.text "unexpected error."]
