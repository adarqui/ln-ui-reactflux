module LN.Input.Profile where



import Data.Maybe



data InputProfile
  = InputProfile_Post
  | InputProfile_Gender String
  | InputProfile_Birthdate String
  | InputProfile_Website (Maybe String)
  | InputProfile_Location (Maybe String)
  | InputProfile_Signature (Maybe String)
  | InputProfile_Nop
