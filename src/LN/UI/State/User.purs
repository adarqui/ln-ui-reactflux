module LN.State.User (
  usersMapLookup,
  usersMapLookup',
  usersMapLookup_ToUser,
  usersMapLookup_ToUser',
  usersMapLookup_ToNick,
  usersMapLookup_ToNick'
) where



import Data.Map                  as M
import Data.Maybe                (Maybe(..), maybe)
import Optic.Core                ((^.), (..))
import Prelude                   (($))

import LN.State.Types            (State)
import LN.T                      (UserSanitizedPackResponse, UserSanitizedResponse
                                 , _UserSanitizedPackResponse, _UserSanitizedResponse
                                 , nick_, user_)



usersMapLookup :: State -> Int -> Maybe UserSanitizedPackResponse
usersMapLookup st = usersMapLookup' st.usersMap



usersMapLookup' :: M.Map Int UserSanitizedPackResponse  -> Int -> Maybe UserSanitizedPackResponse
usersMapLookup' users_map user_id = M.lookup user_id users_map



usersMapLookup_ToUser :: State -> Int -> Maybe UserSanitizedResponse
usersMapLookup_ToUser st = usersMapLookup_ToUser' st.usersMap



usersMapLookup_ToUser' :: M.Map Int UserSanitizedPackResponse -> Int -> Maybe UserSanitizedResponse
usersMapLookup_ToUser' users_map user_id =
  maybe Nothing (\user -> Just $ user ^. _UserSanitizedPackResponse .. user_) $ M.lookup user_id users_map



usersMapLookup_ToNick :: State -> Int -> String
usersMapLookup_ToNick st = usersMapLookup_ToNick' st.usersMap



usersMapLookup_ToNick' :: M.Map Int UserSanitizedPackResponse -> Int -> String
usersMapLookup_ToNick' users_map user_id =
  maybe "unknown" (\user -> user ^. _UserSanitizedResponse .. nick_) (usersMapLookup_ToUser' users_map user_id)
