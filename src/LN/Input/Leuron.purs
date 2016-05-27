module LN.Input.Leuron (
  InputLeuron (..),
  Leuron_Mod (..)
) where



import LN.Internal.Leuron (LeuronSift)



data InputLeuron
  = InputLeuron_Nop1
  | InputLeuron_Set_Sift LeuronSift
  | InputLeuron_Mod Leuron_Mod
  | InputLeuron_Nop



data Leuron_Mod
  = SetTitle String
  | EditTitle String
  | RemTitle

  | SetDescription String
  | EditDescription String
  | RemDescription

  | SetSection String
  | EditSection String
  | RemSection

  | SetPage Int
  | EditPage Int
  | RemPage

  | AddExample String
  | EditExample Int String
  | DelExample Int
  | ClearExamples

  | AddStrength String
  | EditStrength Int String
  | DelStrength Int
  | ClearStrength

  | AddCategory (Array String)
  | EditCategory Int (Array String)
  | DelCategory Int
  | ClearCategories

  -- substitutions
  -- splits

  | AddTag String
  | EditTag Int String
  | DelTag Int
  | ClearTags

  | SetSpecificTo String
  | EditSpecificTo String
  | RemSpecificTo
