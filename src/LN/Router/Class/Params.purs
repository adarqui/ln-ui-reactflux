module LN.Router.Class.Params (
  Params (..)
) where



import Control.Monad.Aff           (Aff())
import Control.Monad.Aff.AVar      (AVAR())
import Control.Monad.Eff.Exception (EXCEPTION())
import Data.Generic                (class Generic, gEq)
import Data.Map                    as M
import Data.Tuple                  (Tuple(..), fst)
import DOM                         (DOM())
import LN.Router.Util              (slash, fixParams)
import Prelude                     (class Eq, class Show, show, (<>), ($), (++), (==))

import LN.T                        (OrderBy(..))



type Params = Array (Tuple String String)
