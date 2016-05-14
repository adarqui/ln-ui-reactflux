module LN.View.Util (
  showIfSelf_UserNick,
  showIfSelf
) where



import Data.Maybe           (Maybe(..))
import Halogen              (HTML)
import Halogen.HTML.Indexed as H
import Optic.Core           ((^.), (..))
import Prelude              ((==))

import LN.State.Types       (State)
import LN.T



showIfSelf_UserNick :: State -> String -> Array (HTML _ _) -> HTML _ _
showIfSelf_UserNick st user_nick html =
  if b
     then H.div_ html
     else H.div_ []
  where
  b = case st.me of
              Nothing -> false
              Just user -> ((user ^. _UserPackResponse .. user_ ^. _UserResponse .. nick_) == user_nick)



showIfSelf :: State -> HTML _ _ -> HTML _ _ -> HTML _ _
showIfSelf st html_self html_else =
  if b
     then html_self
     else html_else
  where
  b = case st.me, st.currentUser of
              Nothing, _ -> false
              _, Nothing -> false
              Just me, Just user ->
                ((me ^. _UserPackResponse .. user_ ^. _UserResponse .. nick_)
                ==
                (user ^. _UserSanitizedPackResponse .. user_ ^. _UserSanitizedResponse .. nick_))
