module LN.Internal.Thread (
  defaultThreadRequest
) where



import Data.Generic                (class Generic)
import Data.Maybe                  (Maybe(Nothing))
import Prelude                     (class Eq, class Show)

import LN.T




defaultThreadRequest :: ThreadRequest
defaultThreadRequest = mkThreadRequest "" Nothing false false Nothing Nothing [] 0
