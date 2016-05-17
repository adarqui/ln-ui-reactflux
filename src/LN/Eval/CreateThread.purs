module LN.Eval.CreateThread (
  eval_CreateThread
) where



import Control.Monad.Aff.Console   (log)
import Data.Either                 (Either(..))
import Data.Functor                (($>))
import Data.Map                    as M
import Data.Maybe                  (Maybe(..), maybe)
import Halogen                     (get, modify, liftAff')
import Optic.Core                  ((^.), (..))
import Prelude                     (bind, pure, show, const, ($), (<>))

import LN.Api                      (rd, getThreadPack', postThread_ByBoardId')
import LN.Component.Types          (EvalEff)
import LN.Input.CreateThread       (InputCreateThread(..))
import LN.Input.Types              (Input(..))
import LN.T                        (_BoardPackResponse
                                   , boardId_
                                   , _ThreadResponse, _ThreadPackResponse
                                   , threadId_
                                   , id_
                                   , mkThreadRequest)



eval_CreateThread :: EvalEff



eval_CreateThread eval (CompCreateThread InputCreateThread_Nop next) = do
  pure next



eval_CreateThread eval (CompCreateThread InputCreateThread_Create next) = do

  st <- get
  let board_id = maybe 0 (\board -> board ^. _BoardPackResponse .. boardId_) st.currentBoard
  let mcomp = st.compCreateThread
  case mcomp of
    Nothing                  -> pure next
    Just comp -> do
      let thread_request = mkThreadRequest comp.name Nothing false false Nothing
      ethread <- rd $ postThread_ByBoardId' board_id thread_request
      case ethread of
        Left err     -> liftAff' $ log ("CreateThread: postThread_ByBoardId': Error: " <> show err) $> next
        Right thread -> do

          -- Now we need to turn this into a thread pack
          ethread_pack <- rd $ getThreadPack' (thread ^. _ThreadResponse .. id_)
          case ethread_pack of
              Left err -> liftAff' $ log ("CreateThread: getThreadPack': Error: " <> show err) $> next
              Right thread_pack -> do

                let
                  thread_id = thread_pack ^. _ThreadPackResponse .. threadId_

                modify (\st' -> st'{ threads = M.update (const $ Just thread_pack) thread_id st.threads })
                eval_CreateThread eval (CompCreateThread (InputCreateThread_SetName Nothing) next)
                pure next



eval_CreateThread eval (CompCreateThread (InputCreateThread_SetName mname) next) = do
  case mname of
       Nothing -> modify (_ { compCreateThread = Nothing }) $> next
       (Just name) -> do
         modify (_ { compCreateThread = Just { name: name } })
         pure next
