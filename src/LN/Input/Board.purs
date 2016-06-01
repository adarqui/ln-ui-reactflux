module LN.Input.Board (
  InputBoard(..),
  Board_Mod(..)
) where



import Data.Maybe           (Maybe)

import LN.T



data InputBoard
  = InputBoard_Mod Board_Mod
  | InputBoard_Nop



data Board_Mod
  = SetTitle String

  | SetDescription String
  | RemoveDescription

  | Save Int
  | EditP Int
