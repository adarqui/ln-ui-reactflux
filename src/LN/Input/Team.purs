module LN.Input.Team (
  InputTeam(..),
  Team_Act(..),
  Team_Mod(..)
) where



import Data.Maybe (Maybe)

import LN.T       (Membership, Visibility)



data InputTeam
  = InputTeam_Act Team_Act
  | InputTeam_Mod Team_Mod
  | InputTeam_Nop



data Team_Act
  = Gets_ByCurrentOrganization
  | GetId                        Int
  | GetSid_ByCurrentOrganization String



data Team_Mod
  = SetMembership Membership
  | SetVisibility Visibility
  | EditP         Int
