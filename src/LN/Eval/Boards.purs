module LN.Eval.Boards (
  eval_GetBoards,
  eval_GetBoardsForForum
) where




import Control.Monad.Aff.Console       (log)
import Data.Array                      (zip)
import Data.Either                     (Either(..))
import Data.Functor                    (($>))
import Data.Map                        as M
import Halogen                         (modify, liftAff')
import Prelude                         (bind, pure, map, show, ($), (<>))

import LN.Api                          (rd, getBoardPacks_ByForumId')
import LN.Component.Types              (EvalEff)
import LN.Input.Types                  (Input(..))
import LN.T                            (BoardPackResponses(..), BoardPackResponse(..))



eval_GetBoards :: EvalEff
eval_GetBoards eval (GetBoards next) = do

  pure next

{- TODO FIXME: do we need this?
  eboardPacks <- rd $ getBoardPacks'
  case eboardPacks of
    Left err -> pure next
    Right (BoardPackResponses boardPacks) -> do
      modify (_{ boards = boardPacks.boardPackResponses })
      pure next
      -}



eval_GetBoardsForForum :: EvalEff
eval_GetBoardsForForum eval (GetBoardsForForum forum_id next) = do

  eboard_packs <- rd $ getBoardPacks_ByForumId' forum_id
  case eboard_packs of
    Left err -> liftAff' $ log ("getBoardPacks_ByForumId: Error: " <> show err) $> next
    Right (BoardPackResponses board_packs) -> do

      let
        boards = board_packs.boardPackResponses
        boards_map = M.fromFoldable $ zip (map (\(BoardPackResponse pack) -> pack.boardId) boards) boards

      modify (_{ boards = boards_map })
      pure next
