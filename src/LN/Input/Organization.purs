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

  | SetIcon           String
  | RemoveIcon

  | SetTag            String -- sets the current tag in state
  | AddTag                   -- adds the current tag in state
  | DeleteTag         Int
  | ClearTags

  | SetMembership     Membership

  | SetVisibility     Visibility

  | Create
  | EditP Int
