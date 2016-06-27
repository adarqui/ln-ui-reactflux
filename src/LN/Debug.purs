module LN.Debug (
  ifDebug,
  ifDebug_ByUser
) where



import Data.Maybe     (Maybe(..))
import Optic.Core     ((^.))
import Prelude        (Unit, unit)

import LN.State.Types (State)
import LN.T           (UserPackResponse(..), _ProfileResponse)



ifDebug :: forall a. State -> (Unit -> a) -> (Unit -> a) -> a
ifDebug st = ifDebug_ByUser st.me



ifDebug_ByUser :: forall a. Maybe UserPackResponse -> (Unit -> a) -> (Unit -> a) -> a
ifDebug_ByUser me t e =
  case me of
    Nothing                    -> e unit
    Just (UserPackResponse me) ->
      let profile = me.profile ^. _ProfileResponse in
      if profile.debug
         then t unit
         else e unit
