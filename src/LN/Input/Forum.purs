module LN.Input.Forum (
  InputForum (..),
  Forum_Mod (..)
) where



import Data.Maybe         (Maybe)

import LN.State.Forum    (ForumRequestState)

import LN.T              (Visibility)



data InputForum
  = InputForum_Nop1
  | InputForum_Mod Forum_Mod
  | InputForum_Nop



data Forum_Mod
  = SetDisplayName String

  | SetDescription String
  | RemoveDescription

  | SetIcon String
  | RemoveIcon

  | AddTag String
  | EditTag Int String
  | DeleteTag Int
  | ClearTags

  | SetVisibility Visibility

  | Create Int  -- save to organization_id
  | EditP Int   -- edit forum_id
