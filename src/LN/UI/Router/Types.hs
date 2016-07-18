{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE FlexibleInstances         #-}

-- |

module LN.UI.Router.Types (
  Routing,
  module CRUD,
  module Link,
  module OrderBy,
  module Routes
) where



import           Data.Maybe                 (Maybe)
import           Prelude                    (Show, IO, show, pure, undefined)

import           LN.UI.Router.Class.CRUD    as CRUD
import           LN.UI.Router.Class.Link    as Link
import           LN.UI.Router.Class.OrderBy as OrderBy
import           LN.UI.Router.Class.Routes  as Routes


import           Data.Text                  (Text)
import           Data.Typeable              (Typeable)
import           React.Flux
-- import qualified Data.JSString.Text as JSS




type Routing e = ()
