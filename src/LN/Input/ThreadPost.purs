module LN.Input.ThreadPost (
  InputThreadPost (..)
) where



import Data.Maybe (Maybe)



data InputThreadPost
  = InputThreadPost_Post
  | InputThreadPost_SetBody (Maybe String)
  | InputThreadPost_Nop
