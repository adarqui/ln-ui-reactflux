module LN.Component.Types (
  ComponentSlot,
  EvalEff,
  EvalEffP,
  LNEff,
  LN
) where



import Control.Monad.Aff         (Aff())
import Control.Monad.Eff.Console (CONSOLE())
import Data.Date                 (Now())
import Data.Date.Locale          (Locale())
import Halogen                   (HalogenEffects, Component, Eval)
import Network.HTTP.Affjax       (AJAX())
import Prelude                   (Unit)
import WebSocket                 (WEBSOCKET())
import Browser.WebStorage        (WebStorage())

import LN.Input.Types            (Input)
import LN.State.Types            (State)



type ComponentSlot s f g = Unit -> { component :: Component s f g, initialState :: s }



type EvalEff = forall eff. {-Partial =>-} Eval Input State Input (LNEff eff) -> Eval Input State Input (LNEff eff)
type EvalEffP= forall eff. Eval Input State Input (LNEff eff)



type LNEff eff = Aff (LN eff)
type LN eff =
  HalogenEffects
    (webStorage :: WebStorage
    , ajax      :: AJAX
    , now       :: Now
    , locale    :: Locale
    , ws        :: WEBSOCKET
    , console   :: CONSOLE | eff
    )
