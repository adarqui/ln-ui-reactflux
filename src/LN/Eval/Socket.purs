module LN.Eval.Socket (
  eval_ConnectSocket,
  makeSocket
) where



import Control.Monad.Aff                (Aff())
import Control.Monad.Aff.Console        (log)
import Control.Monad.Aff.AVar           (AVAR(), AVar(..), putVar)
import Control.Monad.Eff.Class          (liftEff)
import Control.Monad.Eff.Console        as Console
import Control.Monad.Eff.Console.Unsafe as Console
import Control.Monad.Eff.Var            (($=), get)
import Halogen                          (gets, action)
import Prelude                          (Unit, unit, bind, pure, ($), (++))
import WebSocket                        (URL(..), WEBSOCKET(), Connection(..), newWebSocket
                                        , runMessage, runMessageEvent)

import LN.Component.Types               (EvalEff)
import LN.Component.Util                (quietLaunchAff)
import LN.Input.Types                   (Input(..))

import Control.Monad.Aff.Free (fromAff)


-- alot of this code from:
-- https://github.com/nathanic/purescript-simple-chat-client/blob/master/src/Main.purs



--  Connection socket <- newWebSocket (URL "wss://leuro.adarq.org/spa") []
eval_ConnectSocket :: EvalEff
eval_ConnectSocket eval (ConnectSocket next) = do
  fromAff $ log "ConnectSocket"
-- TODO FIXME: took this out temporarily because of cyclic dependency  ch <- gets _.driverCh
-- TODO FIXME: ^^  fromAff $ makeSocket ch (URL "wss://leuro.adarq.org")
--  Connection socket <- liftEff' $ newWebSocket (URL "wss://leuro.adarq.org/spa") []
--  driver <- makeAuxDriver <$> get
--  url <- URL <$> gets _.chatServerUrl
--  fromAff $ makeSocket driver "wss://leuro.adarq.org/spa"
  pure next



-- makeAuxDriver :: forall r. {queryChan :: AVar (Input Unit) | r} -> AppDriver
-- makeAuxDriver {queryChan=chan} = putVar chan



--makeSocket :: forall eff. AppDriver -> URL -> Aff (avar :: AVAR, ws :: WEBSOCKET | eff) Unit
--makeSocket driver url = do
makeSocket :: forall eff. AVar (Input Unit) -> URL -> Aff (avar :: AVAR, ws :: WEBSOCKET | eff) Unit
makeSocket ch url = do
  liftEff do
    conn@(Connection socket) <- newWebSocket url []

    socket.onopen $= \event -> do
      Console.logAny event
      Console.log "onopen: Connection opened"
--            quietLaunchAff $ driver $ action $ Connect conn

    socket.onmessage $= \event -> do
      Console.logAny event
      let received = runMessage (runMessageEvent event)
      Console.log $ "onmessage: Received '" ++ received ++ "'"
      quietLaunchAff $ putVar ch (action Nop)
--            quietLaunchAff $ driver $ action $ ReceivedMessage received

    socket.onclose $= \event -> do
      Console.logAny event
      Console.log "onclose: Connection closed"
--            quietLaunchAff $ driver $ action $ Disconnect

  pure unit
