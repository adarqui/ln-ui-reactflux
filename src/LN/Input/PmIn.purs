module LN.Input.PmIn (
  InputPmIn(..),
  PmIn_Act(..),
  PmIn_Mod(..)
) where



import Data.Maybe           (Maybe)

import LN.T



data InputPmIn
  = InputPmIn_Act PmIn_Act
  | InputPmIn_Mod PmIn_Mod
  | InputPmIn_Nop



data PmIn_Act
  = Act_Nop



data PmIn_Mod
  = Mod_Nop
