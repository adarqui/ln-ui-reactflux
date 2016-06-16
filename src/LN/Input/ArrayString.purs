module LN.Input.ArrayString (
  InputArrayString(..),
  ArrayStringEnt(..)
) where



import Prelude              (class Show, class Eq, class Ord, Ordering(..))



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



instance arrayStringEntEq :: Eq ArrayStringEnt where
  eq ASE_Tags          ASE_Tags          = true
  eq ASE_PrivateTags   ASE_PrivateTags   = true
  eq ASE_SuggestedTags ASE_SuggestedTags = true
  eq ASE_Examples      ASE_Examples      = true
  eq _                 _                 = false



instance arrayStringEntOrd :: Ord ArrayStringEnt where
  compare ASE_Tags          ASE_Tags          = EQ
  compare ASE_PrivateTags   ASE_PrivateTags   = EQ
  compare ASE_SuggestedTags ASE_SuggestedTags = EQ
  compare ASE_Examples      ASE_Examples      = EQ



data InputArrayString
  = SetCurrent    ArrayStringEnt String
  | AddToTags     ArrayStringEnt
  | DeleteTag     ArrayStringEnt Int
  | ClearTags     ArrayStringEnt
