module LN.Eval.Teams (
  eval_GetTeams
) where



import Data.Either                     (Either(..))
import Halogen                         (modify)
import Prelude                         (bind, pure, ($))

import LN.Api                          (rd, getTeamPacks')
import LN.Component.Types              (EvalEff)
import LN.Helpers.Map                  (idmapFrom)
import LN.Input.Types                  (Input(..))
import LN.T                            (TeamPackResponses(..), TeamPackResponse(..))



eval_GetTeams :: EvalEff
eval_GetTeams eval (GetTeams next) = do

  e_teams <- rd $ getTeamPacks'
  case e_teams of
    Left err                             -> eval (AddErrorApi "eval_GetTeams::getTeamPacks'" err next)
    Right (TeamPackResponses team_packs) -> do

      let
        teams     = team_packs.teamPackResponses
        teams_map = idmapFrom (\(TeamPackResponse p) -> p.teamId) team_packs.teamPackResponses

      modify (_{ teams = teams_map })
      pure next
