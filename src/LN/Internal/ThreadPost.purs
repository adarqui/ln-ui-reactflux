module LN.Internal.ThreadPost (
  defaultThreadPostRequest
) where



import Data.Generic                (class Generic)
import Data.Maybe                  (Maybe(Nothing))
import Prelude                     (class Eq, class Show)

import LN.T




defaultThreadPostRequest :: ThreadPostRequest
defaultThreadPostRequest = mkThreadPostRequest Nothing (PostDataBBCode "") [] [] 0
