module LN.Input.Membership (
  InputMembership(..),
  Membership_Act(..),
  Membership_Mod(..)
) where



data InputMembership
  = InputMembership_Act Membership_Act
  | InputMembership_Mod Membership_Mod
  | InputMembership_Nop



data Membership_Act
  = Join_ByCurrentOrganization
  | Leave_ByCurrentOrganization



data Membership_Mod
  = Nop
