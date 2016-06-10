module LN.Input.ThreadPost (
  InputThreadPost (..),
  ThreadPost_Mod (..)
) where



import Data.Maybe (Maybe)



data InputThreadPost
  = InputThreadPost_Mod ThreadPost_Mod
  | InputThreadPost_Nop



data ThreadPost_Mod
  = SetTitle String
  | RemoveTitle

  | SetBody String
  | RemoveBody

  | AddTag String
  | EditTag Int String
  | DeleteTag Int
  | ClearTags

  | AddPrivateTag String
  | EditPrivateTag Int String
  | DeletePrivateTag Int
  | ClearPrivateTags

  | Create Int -- TODO FIXME: create based on thread_id or thread_post_id
  | EditP Int  -- via thread_post_id
