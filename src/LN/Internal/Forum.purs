module LN.Internal.Forum (
  defaultForumRequest
) where



import Data.Generic                (class Generic)
import Data.Maybe                  (Maybe(Nothing))
import Prelude                     (class Eq, class Show)

import LN.T




defaultForumRequest :: ForumRequest
defaultForumRequest = mkForumRequest "" Nothing 20 20 Nothing [] Public 0
