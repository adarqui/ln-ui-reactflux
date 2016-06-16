module LN.Input.Organization (
  InputOrganization(..),
  Organization_Act(..),
  Organization_Mod(..)
) where



import Data.Maybe           (Maybe)

import LN.T                 (Membership, Visibility)



data InputOrganization
  = InputOrganization_Act Organization_Act
  | InputOrganization_Mod Organization_Mod
  | InputOrganization_Nop



data Organization_Act
  = Gets
  | GetId  Int
  | GetSid String



data Organization_Mod
  = SetDisplayName    String

  | SetDescription    String
  | RemoveDescription

  | SetCompany        String

  | SetLocation       String

  | SetMembership     Membership
  | SetVisibility     Visibility

  | Create
  | EditP Int
