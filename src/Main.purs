module Main where



import LN.Component.Types          (LN)
import LN.Component                as Q
import LN.Input.Types              (Input(..))
import LN.State.Types              as S
import Control.Monad.Aff           (runAff, forkAff)
import Control.Monad.Aff.AVar      (makeVar, takeVar)
import Control.Monad.Eff           (Eff())
import Control.Monad.Eff.Exception (throwException)
import Control.Monad.Rec.Class     (forever)
import Halogen                     (runUI, action)
import Halogen.Util                (appendToBody, onLoad, appendTo)
import Prelude                     (Unit, unit, const, pure, bind, ($), (>>=))
import Router                      as R



main :: forall eff. {-Partial =>-} Eff (LN eff) Unit
main = do

  runAff throwException (const (pure unit)) do

    ch <- makeVar

    app <- runUI Q.ui (S.initialState ch)
    onLoad $ appendTo "#purescript-container" app.node
    forkAff $ R.routeSignal app.driver
    app.driver (action GetMe)
    app.driver (action ConnectSocket)

    -- for web socket
    forever (takeVar ch >>= app.driver)
