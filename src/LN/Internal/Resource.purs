module LN.Internal.Resource (
  RType(..),
  resourceTypeToRType
) where



import Data.Generic                (class Generic)
import Prelude                     (class Eq, class Show, eq, show)

import LN.T



-- | The state of the component.
data RType = RTypeNONE | RTypeURL | RTypeISBN

derive instance genericRType :: Generic RType



instance rtypeShow :: Show RType  where
  show RTypeNONE = "none"
  show RTypeURL  = "url"
  show RTypeISBN = "isbn"



instance rtypeEq :: Eq RType where
  eq RTypeNONE RTypeNONE = true
  eq RTypeURL  RTypeURL  = true
  eq RTypeISBN RTypeISBN = true
  eq _      _      = false



resourceTypeToRType :: ResourceType -> RType
resourceTypeToRType type_ =
  case type_ of
       ISBN13 _   -> RTypeISBN
       ISBN10 _   -> RTypeISBN
       ISBN _     -> RTypeISBN
       URL _      -> RTypeURL
       SourceNone -> RTypeNONE
