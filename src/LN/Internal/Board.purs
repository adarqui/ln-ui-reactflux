module LN.Internal.Board (
  defaultBoardRequest
) where



import Data.Generic                (class Generic)
import Data.Maybe                  (Maybe(Nothing))
import Prelude                     (class Eq, class Show)

import LN.T




defaultBoardRequest :: BoardRequest
defaultBoardRequest = mkBoardRequest "" Nothing Nothing [] 0
