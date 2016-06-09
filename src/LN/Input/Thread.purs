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
  = SetDisplayName String

  | SetDescription String
  | RemoveDescription

  | SetSticky Boolean

  | SetLocked Boolean

  | SetPoll Boolean

  | SetIcon String
  | RemoveIcon

  | Create Int  -- via board_id
  | EditP Int   -- via thread_id
