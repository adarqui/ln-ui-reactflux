module LN.Input.Pm (
  InputPm(..),
  Pm_Act(..),
  Pm_Mod(..)
) where



import Data.Maybe           (Maybe)

import LN.T



data InputPm
  = InputPm_Act Pm_Act
  | InputPm_Mod Pm_Mod
  | InputPm_Nop



data Pm_Act
  = Nop



data Pm_Mod
  = SetSubject    String
  | SetBody       String
  | Send          Int -- user id
  | EditP         Int -- pm id
