module LN.Input.ThreadPost where



import Data.Maybe



data InputThreadPost
  = InputThreadPost_Post
  | InputThreadPost_SetBody (Maybe String)
  | InputThreadPost_Nop
