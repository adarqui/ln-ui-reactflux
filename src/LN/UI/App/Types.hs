module LN.UI.App.Types (
  UsersMap
) where



import           Data.Int                 (Int64)
import           Data.Map                 (Map)
import qualified Data.Map                 as Map

import           LN.T.Pack.Sanitized.User (UserSanitizedPackResponse)



type UsersMap = Map Int64 UserSanitizedPackResponse
