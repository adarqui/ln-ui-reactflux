module LN.State.User (
  usersMapLookup,
  usersMapLookup_ToUser,
  usersMapLookup_ToNick,
) where



import Data.Map                  as M
import Data.Maybe                (Maybe(..), maybe)
import Optic.Core                (LensP, lens, (^.), (..))
import Prelude                   (($))

import LN.Component.CreateThread (Comp_CreateThread_State)
import LN.Router.Types           (Routes)
import LN.State.PageInfo         (PageInfo)
import LN.State.Types            (State)
import LN.T



usersMapLookup :: State -> Int -> Maybe UserSanitizedPackResponse
usersMapLookup st user_id =
  M.lookup user_id st.usersMap



usersMapLookup_ToUser :: State -> Int -> Maybe UserSanitizedResponse
usersMapLookup_ToUser st user_id =
  maybe Nothing (\user -> Just $ user ^. _UserSanitizedPackResponse .. user_) $ M.lookup user_id st.usersMap



usersMapLookup_ToNick :: State -> Int -> String
usersMapLookup_ToNick st user_id =
  maybe "unknown" (\user -> user ^. _UserSanitizedResponse .. nick_) (usersMapLookup_ToUser st user_id)
