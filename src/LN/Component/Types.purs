module LN.Component.Types (
  ComponentSlot,
  CompEff,
  EvalEff,
--  EvalEffP,
  LNEff,
  LN
) where



import Control.Monad.Aff         (Aff())
import Control.Monad.Eff.Console (CONSOLE())
import Data.Date                 (Now())
import Data.Date.Locale          (Locale())
import Data.NaturalTransformation (Natural(..))
import Halogen                   (HalogenEffects, Component, ComponentDSL)
import Network.HTTP.Affjax       (AJAX())
import Prelude                   (Unit)
import WebSocket                 (WEBSOCKET())
import Browser.WebStorage        (WebStorage())

import LN.Input.Types            (Input)
import LN.State.Types            (State)



type ComponentSlot s f g = Unit -> { component :: Component s f g, initialState :: s }



-- type EvalEff = forall eff. {-Partial =>-} Natural Input State Input (LNEff eff) -> Natural Input State Input (LNEff eff)
type CompEff = forall eff. Natural Input (ComponentDSL State Input (LNEff eff))
type EvalEff = CompEff -> CompEff
-- type EvalEff = forall eff. Natural Input (ComponentDSL State Input (LNEff eff)) -> Natural Input (ComponentDSL State Input (LNEff eff))
-- Natural Query (ComponentDSL State Query (Aff (AppEffects eff)))
--type EvalEffP = forall eff. Natural Input State Input (LNEff eff)



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
