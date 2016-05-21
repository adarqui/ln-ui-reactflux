module LN.Helpers.Log (
  log
) where



import Control.Monad.Aff         (Aff())
import Control.Monad.Aff.Console as C
import Control.Monad.Eff.Console (CONSOLE())
import Prelude                   (Unit)



-- | made this in case we want to turn logging off in the future, ie, when we go into prod
--
log :: forall e. String -> Aff (console :: CONSOLE | e) Unit
log = C.log
