module LN.Input.CreateThread where



import Data.Maybe



data InputCreateThread
  = InputCreateThread_Create
  | InputCreateThread_SetName (Maybe String)
  | InputCreateThread_Nop
