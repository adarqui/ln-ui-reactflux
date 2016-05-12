module LN.Input.CreateThread (
  InputCreateThread (..)
) where



import Data.Maybe (Maybe)



data InputCreateThread
  = InputCreateThread_Create
  | InputCreateThread_SetName (Maybe String)
  | InputCreateThread_Nop
