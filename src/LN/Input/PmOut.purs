module LN.Input.PmOut (
  InputPmOut(..),
  PmOut_Act(..),
  PmOut_Mod(..)
) where



import Data.Maybe           (Maybe)

import LN.T



data InputPmOut
  = InputPmOut_Act PmOut_Act
  | InputPmOut_Mod PmOut_Mod
  | InputPmOut_Nop



data PmOut_Act
  = Act_Nop



data PmOut_Mod
  = Mod_Nop
