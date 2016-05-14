module LN.Eval.Boards (
  eval_GetBoards,
  eval_GetBoardsForForum
) where




import Control.Monad.Aff.Console       (log)
import Data.Either                     (Either(..))
import Data.Functor                    (($>))
import Halogen                         (modify, liftAff')
import Prelude                         (bind, pure, show, ($), (<>))

import LN.Api                          (rd, getBoardPacks', getBoardPacks_ByForumId')
import LN.Component.Types              (EvalEff)
import LN.Input.Types                  (Input(..))
import LN.T



eval_GetBoards :: EvalEff
eval_GetBoards eval (GetBoards next) = do

  eboardPacks <- rd $ getBoardPacks'
  case eboardPacks of
    Left err -> pure next
    Right (BoardPackResponses boardPacks) -> do
      modify (_{ boards = boardPacks.boardPackResponses })
      pure next



eval_GetBoardsForForum :: EvalEff
eval_GetBoardsForForum eval (GetBoardsForForum forum_id next) = do

  eboardPacks <- rd $ getBoardPacks_ByForumId' forum_id
  case eboardPacks of
    Left err -> liftAff' $ log ("getBoardPacks_ByForumId: Error: " <> show err) $> next
    Right (BoardPackResponses boardPacks) -> do
      modify (_{ boards = boardPacks.boardPackResponses })
      pure next
