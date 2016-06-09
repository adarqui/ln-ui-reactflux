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
  = SetDisplayName String

  | SetDescription String
  | RemoveDescription

  | SetIcon String
  | RemoveIcon

  | AddTag String
  | EditTag Int String
  | DeleteTag Int
  | ClearTags

  | Create Int -- TODO FIXME: needs to be a forum id OR board id
  | EditP Int  -- edit board_id
