module LN.Eval.GetBoardPacks (
  eval_GetBoardPacks,
  eval_GetBoardPacksForForum
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



eval_GetBoardPacks :: EvalEff
eval_GetBoardPacks eval (GetBoardPacks next) = do

  eboardPacks <- rd $ getBoardPacks'
  case eboardPacks of
    Left err -> pure next
    Right (BoardPackResponses boardPacks) -> do
      modify (_{ boards = boardPacks.boardPackResponses })
      pure next



eval_GetBoardPacksForForum :: EvalEff
eval_GetBoardPacksForForum eval (GetBoardPacksForForum forum_id next) = do

  eboardPacks <- rd $ getBoardPacks_ByForumId' forum_id
  case eboardPacks of
    Left err -> liftAff' $ log ("getBoardPacks_ByForumId: Error: " <> show err) $> next
    Right (BoardPackResponses boardPacks) -> do
      modify (_{ boards = boardPacks.boardPackResponses })
      pure next
