module LN.Eval.Boards (
  eval_GetBoards,
  eval_GetBoardsForForum
) where



import Data.Either                     (Either(..))
import Halogen                         (modify)
import Prelude                         (bind, pure, ($))

import LN.Api                          (rd, getBoardPacks_ByForumId')
import LN.Component.Types              (EvalEff)
import LN.Input.Types                  (Input(..))
import LN.Helpers.Map                  (idmapFrom)
import LN.T                            (BoardPackResponses(..), BoardPackResponse(..))



eval_GetBoards :: EvalEff
eval_GetBoards eval (GetBoards next) = pure next



eval_GetBoardsForForum :: EvalEff
eval_GetBoardsForForum eval (GetBoardsForForum forum_id next) = do

  e_board_packs <- rd $ getBoardPacks_ByForumId' forum_id
  case e_board_packs of
    Left err -> eval (AddErrorApi "eval_GetBoardsForForum" err next)
    Right (BoardPackResponses board_packs) -> do

      let
        boards_map = idmapFrom (\(BoardPackResponse pack) -> pack.boardId) board_packs.boardPackResponses

      modify (_{ boards = boards_map })
      pure next
