module LN.Input.Leuron (
  InputLeuron (..),
  Leuron_Mod (..)
) where



import Data.Maybe         (Maybe)

import LN.T               (TyLeuron, LeuronData)
import LN.Internal.Leuron (LeuronSift)
import LN.State.Leuron    (LeuronRequestState)



data InputLeuron
  = InputLeuron_Nop1
  | InputLeuron_Set_Sift LeuronSift
  | InputLeuron_Mod Leuron_Mod
  | InputLeuron_Nop



data Leuron_Mod
  = SetTitle String
  | EditTitle String
  | RemoveTitle

  | SetDescription String
  | EditDescription String
  | RemoveDescription

  | SetSection String
  | EditSection String
  | RemoveSection

  | SetPage Int
  | EditPage Int
  | RemovePage

  | AddExample String
  | EditExample Int String
  | DeleteExample Int
  | ClearExamples

  | AddStrength String
  | EditStrength Int String
  | DeleteStrength Int
  | ClearStrength

  | AddCategory (Array String)
  | EditCategory Int (Array String)
  | DeleteCategory Int
  | ClearCategories

  -- substitutions
  -- splits

  | AddTag String
  | EditTag Int String
  | DeleteTag Int
  | ClearTags

  | SetSpecificTo String
  | EditSpecificTo String
  | RemoveSpecificTo

  | SetData LeuronData
  | SetType TyLeuron
  | SetSt LeuronRequestState

  | Save Int -- save to resource_id
  | Edit Int -- edit leuron_id
