module LN.Input.ArrayString (
  InputArrayString(..),
  ArrayStringEnt(..)
) where



import Prelude              (class Show)



data ArrayStringEnt
  = ASE_Tags
  | ASE_PrivateTags
  | ASE_SuggestedTags
  | ASE_Examples



instance arrayStringEntShow :: Show ArrayStringEnt where
  show ASE_Tags          = "tags"
  show ASE_PrivateTags   = "private_tags"
  show ASE_SuggestedTags = "suggested_tags"
  show ASE_Examples      = "examples"



data InputArrayString
  = SetCurrentTag ArrayStringEnt String
  | AddToTags     ArrayStringEnt
  | DeleteTag     ArrayStringEnt Int
  | ClearTags     ArrayStringEnt
