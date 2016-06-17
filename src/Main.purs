module Main where



import Control.Monad.Aff           (runAff, forkAff)
import Control.Monad.Aff.AVar      (makeVar, takeVar)
import Control.Monad.Eff           (Eff())
import Control.Monad.Eff.Exception (throwException)
import Control.Monad.Rec.Class     (forever)
import Data.Maybe                  (Maybe(..))
import Halogen                     (runUI, action)
import Halogen.Util                (awaitLoad, awaitBody, selectElement)
import Prelude                     (Unit, unit, const, pure, bind, ($), (>>=))
import Router                      as R

import LN.Component.Types          (LN)
import LN.Component                as Q
import LN.Input.Types              (Input(..))
import LN.State.Types              as S


main :: forall eff. {-Partial =>-} Eff (LN eff) Unit
main = do

  runAff throwException (const (pure unit)) do

    ch <- makeVar

    body   <- awaitBody
    m_node <- selectElement "#purescript-container"
    let node = (case m_node of
         Nothing   -> body
         Just node -> node)

    driver <- runUI Q.ui (S.initialState ch) body
    forkAff $ R.routeSignal driver
    driver (action GetMe)
    driver (action ConnectSocket)

    -- for web socket
    forever (takeVar ch >>= driver)
    pure unit
