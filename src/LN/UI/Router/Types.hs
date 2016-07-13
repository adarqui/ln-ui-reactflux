module LN.UI.Router.Types (
  Routing,
  module CRUD,
  module Link,
  module OrderBy,
  module Routes
) where



import           Prelude                    (undefined)

import           LN.UI.Router.Class.CRUD    as CRUD
import           LN.UI.Router.Class.Link    as Link
import           LN.UI.Router.Class.OrderBy as OrderBy
import           LN.UI.Router.Class.Routes  as Routes



type Routing e = ()
