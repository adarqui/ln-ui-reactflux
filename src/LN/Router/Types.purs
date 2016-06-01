module LN.Router.Types (
  Routing,
  module CRUD,
  module Link,
  module OrderBy,
  module Routes
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
import LN.Router.Class.CRUD        as CRUD
import LN.Router.Class.Link        as Link
import LN.Router.Class.OrderBy     as OrderBy
import LN.Router.Class.Routes      as Routes



type Routing e = Aff (dom :: DOM, avar :: AVAR, err :: EXCEPTION | e)
