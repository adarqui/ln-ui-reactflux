module LN.Input.TeamMember (
  InputTeamMember(..),
  TeamMember_Act(..),
  TeamMember_Mod(..)
) where



import Data.Maybe           (Maybe)

import LN.T



data InputTeamMember
  = InputTeamMember_Act TeamMember_Act
  | InputTeamMember_Mod TeamMember_Mod
  | InputTeamMember_Nop



data TeamMember_Act
  = Gets_ByCurrentTeam
  | GetId              Int



data TeamMember_Mod
  = Nop
