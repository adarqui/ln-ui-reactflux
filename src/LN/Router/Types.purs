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
import DOM                         (DOM())

import LN.Router.Class.CRUD        as CRUD
import LN.Router.Class.Link        as Link
import LN.Router.Class.OrderBy     as OrderBy
import LN.Router.Class.Routes      as Routes



type Routing e = Aff (dom :: DOM, avar :: AVAR, err :: EXCEPTION | e)
