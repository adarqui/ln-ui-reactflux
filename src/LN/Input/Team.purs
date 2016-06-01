module LN.Input.Team (
  InputTeam(..),
  Team_Mod(..)
) where



import Data.Maybe           (Maybe)

import LN.T



data InputTeam
  = InputTeam_Mod Team_Mod
  | InputTeam_Nop



data Team_Mod
  = SetTitle String

  | SetDescription String
  | RemoveDescription

  | Save Int
  | EditP Int
