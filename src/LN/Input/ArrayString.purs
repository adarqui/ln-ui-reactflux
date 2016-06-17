module LN.Input.ArrayString (
  InputArrayString(..),
  ArrayStringEnt(..)
) where



import Data.Enum            (class Enum, Cardinality(..), fromEnum, defaultSucc, defaultPred)
-- import Data.Function        (on)
import Data.Maybe           (Maybe(..))
import Prelude              ( class Show
                            , class Eq
                            , class Ord, Ordering(..), compare
                            , class Bounded
                            , class BoundedOrd)



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
--  compare = compare `on` fromEnum
  compare _ _ = EQ



instance arrayStringEntBounded :: Bounded ArrayStringEnt where
  bottom = ASE_Tags
  top    = ASE_Examples



instance arrayStringEntBoundedOrd :: BoundedOrd ArrayStringEnt



instance arrayStringEntEnum :: Enum ArrayStringEnt where
  cardinality = Cardinality 4
  succ        = defaultSucc arrayStringEntToEnum arrayStringEntFromEnum
  pred        = defaultPred arrayStringEntToEnum arrayStringEntFromEnum
  toEnum      = arrayStringEntToEnum
  fromEnum    = arrayStringEntFromEnum

arrayStringEntToEnum :: Int -> Maybe ArrayStringEnt
arrayStringEntToEnum 0 = Just ASE_Tags
arrayStringEntToEnum 1 = Just ASE_PrivateTags
arrayStringEntToEnum 2 = Just ASE_SuggestedTags
arrayStringEntToEnum 3 = Just ASE_Examples

arrayStringEntFromEnum :: ArrayStringEnt -> Int
arrayStringEntFromEnum ASE_Tags          = 0
arrayStringEntFromEnum ASE_PrivateTags   = 1
arrayStringEntFromEnum ASE_SuggestedTags = 2
arrayStringEntFromEnum ASE_Examples      = 3



data InputArrayString
  = SetCurrent            ArrayStringEnt String
  | AddFromCurrent        ArrayStringEnt
  | AddFromCurrentSort    ArrayStringEnt
  | AddFromCurrentNub     ArrayStringEnt
  | AddFromCurrentSortNub ArrayStringEnt
  | Edit                  ArrayStringEnt Int String
  | Delete                ArrayStringEnt Int
  | Clear                 ArrayStringEnt
  | Empty
