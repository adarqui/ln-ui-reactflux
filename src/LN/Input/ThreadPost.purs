module LN.Input.ThreadPost (
  InputThreadPost(..),
  ThreadPost_Act(..),
  ThreadPost_Mod(..)
) where



import Data.Maybe (Maybe)



data InputThreadPost
  = InputThreadPost_Act ThreadPost_Act
  | InputThreadPost_Mod ThreadPost_Mod
  | InputThreadPost_Nop



data ThreadPost_Act
  = Gets
  | Gets_ByCurrentThread
  | Gets_ByCurrentThread_And_ThreadPostId Int
  | GetId                                 Int
  | GetSid_ByCurrentThread                String
  | ResyncById                            Int



data ThreadPost_Mod
  = SetTitle         String
  | RemoveTitle

  | SetBody          String
  | RemoveBody

  | SetPrivateTag    String
  | AddPrivateTag
  | DeletePrivateTag Int
  | ClearPrivateTags

  | SetTag           String
  | AddTag
  | DeleteTag        Int
  | ClearTags

  | Create           Int -- TODO FIXME: create based on thread_id or thread_post_id
  | EditP            Int  -- via thread_post_id
