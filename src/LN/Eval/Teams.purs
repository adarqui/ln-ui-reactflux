module LN.Eval.Teams (
  eval_GetTeams
) where



import Data.Array                      (zip)
import Data.Either                     (Either(..))
import Data.Map                        as M
import Halogen                         (modify)
import Optic.Core                      ((^.), (..))
import Prelude                         (bind, pure, map, ($))

import LN.Api                          (rd, getTeamPacks')
import LN.Component.Types              (EvalEff)
import LN.Input.Types                  (Input(..))
import LN.T                            (TeamPackResponses(..), TeamPackResponse(..)
                                       , _TeamResponse, id_)



eval_GetTeams :: EvalEff
eval_GetTeams eval (GetTeams next) = do

  eteams <- rd $ getTeamPacks'
  case eteams of
    Left err -> pure next
    Right (TeamPackResponses team_packs) -> do

      let
        teams     = team_packs.teamPackResponses
        teams_map = M.fromFoldable $ zip (map (\(TeamPackResponse pack) -> pack.team ^. _TeamResponse .. id_) teams) teams

      modify (_{ teams = teams_map })
      pure next
