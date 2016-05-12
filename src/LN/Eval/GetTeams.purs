module LN.Eval.GetTeams (
  eval_GetTeams
) where



import Data.Either                     (Either(..))
import Halogen                         (modify)
import Prelude                         (bind, pure, ($))

import LN.Api                          (rd, getTeams')
import LN.Component.Types              (EvalEff)
import LN.Input.Types                  (Input(..))
import LN.T



eval_GetTeams :: EvalEff
eval_GetTeams eval (GetTeams next) = do

  eteams <- rd $ getTeams'
  case eteams of
    Left err -> pure next
    Right (TeamResponses teams) -> do
      modify (_{ teams = teams.teamResponses })
      pure next
