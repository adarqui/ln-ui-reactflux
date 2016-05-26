module LN.Internal.Resource (
  RType(..),
  resourceTypeToRType,
  defaultResourceRequest
) where



import Data.Generic                (class Generic)
import Data.Maybe                  (Maybe(Nothing))
import Prelude                     (class Eq, class Show)

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



defaultResourceRequest :: ResourceRequest
defaultResourceRequest = mkResourceRequest "" "" SourceNone Nothing [] [] Public 0 Nothing Nothing
