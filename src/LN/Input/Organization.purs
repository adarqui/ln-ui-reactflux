module LN.Input.Organization (
  InputOrganization(..),
  Organization_Mod(..)
) where



import Data.Maybe           (Maybe)

import LN.T



data InputOrganization
  = InputOrganization_Mod Organization_Mod
  | InputOrganization_Nop



data Organization_Mod
  = SetTitle String

  | SetDescription String
  | RemoveDescription

  | SetCompany String

  | SetLocation String

  | SetEmail String

  | Save Int
  | EditP Int
