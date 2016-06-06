module LN.Internal.Resource (
  resourceTypeToTyResourceType,
  defaultResourceRequest,
  unwrapResourceSource
) where



import Data.Generic                (class Generic)
import Data.Maybe                  (Maybe(Nothing))
import Prelude                     (class Eq, class Show)

import LN.T



resourceTypeToTyResourceType :: ResourceType -> TyResourceType
resourceTypeToTyResourceType type_ =
  case type_ of
       ISBN13 _   -> TyISBN
       ISBN10 _   -> TyISBN
       ISBN _     -> TyISBN
       URL _      -> TyURL
       SourceNone -> TySourceNone



defaultResourceRequest :: ResourceRequest
defaultResourceRequest = mkResourceRequest "" "" SourceNone Nothing [] [] Public 0 Nothing Nothing Nothing [] 0



unwrapResourceSource :: ResourceType -> String
unwrapResourceSource source =
  case source of
       ISBN13 s -> s
       ISBN10 s -> s
       ISBN   s -> s
       URL    s -> s
       _        -> ""
