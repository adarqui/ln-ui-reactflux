module LN.Input.Thread (
  InputThread(..),
  Thread_Act(..),
  Thread_Mod(..)
) where



import Data.Maybe           (Maybe)

import LN.T



data InputThread
  = InputThread_Act Thread_Act
  | InputThread_Mod Thread_Mod
  | InputThread_Nop



data Thread_Act
  = Gets
  | Gets_ByCurrentBoard
  | GetId                 Int
  | GetSid_ByCurrentBoard String



data Thread_Mod
  = SetDisplayName    String

  | SetDescription    String
  | RemoveDescription

  | SetSticky         Boolean

  | SetLocked         Boolean

  | SetPoll           Boolean

  | SetIcon           String
  | RemoveIcon

  | Create            Int -- via board_id
  | EditP             Int -- via thread_id
