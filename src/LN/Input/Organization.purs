module LN.Input.Organization (
  InputOrganization(..),
  Organization_Mod(..),
  Organization_Act(..)
) where



import Data.Maybe           (Maybe)

import LN.T



data InputOrganization
  = InputOrganization_Mod Organization_Mod
  | InputOrganization_Act Organization_Act
  | InputOrganization_Nop



data Organization_Mod
  = SetDisplayName    String

  | SetDescription    String
  | RemoveDescription

  | SetCompany        String

  | SetLocation       String

  | Create
  | EditP Int



data Organization_Act
  = Nop
  | Gets
  | GetId  Int
  | GetSid String
