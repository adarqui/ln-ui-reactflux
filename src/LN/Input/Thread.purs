module LN.Input.Thread (
  InputThread(..),
  Thread_Mod(..)
) where



import Data.Maybe           (Maybe)

import LN.T



data InputThread
  = InputThread_Mod Thread_Mod
  | InputThread_Nop



data Thread_Mod
  = SetName String

  | SetDescription String
  | RemoveDescription

  | SetSticky Boolean

  | SetLocked Boolean

  | SetPoll Boolean

  | Save Int
  | EditP Int
