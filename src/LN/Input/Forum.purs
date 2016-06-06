module LN.Input.Forum (
  InputForum (..),
  Forum_Mod (..)
) where



import Data.Maybe         (Maybe)

import LN.State.Forum    (ForumRequestState)



data InputForum
  = InputForum_Nop1
  | InputForum_Mod Forum_Mod
  | InputForum_Nop



data Forum_Mod
  = SetDisplayName String

  | SetDescription String

  | Save Int -- save to organization_id
  | EditP Int -- edit forum_id
