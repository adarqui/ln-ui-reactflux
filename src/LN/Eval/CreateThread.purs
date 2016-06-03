module LN.Eval.CreateThread (
  eval_CreateThread
) where



import Data.Either                 (Either(..))
import Data.Functor                (($>))
import Data.Map                    as M
import Data.Maybe                  (Maybe(..))
import Halogen                     (get, modify)
import Optic.Core                  ((^.), (..))
import Prelude                     (bind, pure, const, ($))

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



eval_CreateThread eval (CompCreateThread InputCreateThread_Nop next) = pure next



eval_CreateThread eval (CompCreateThread InputCreateThread_Create next) = do

  st <- get
  case st.currentBoard, st.compCreateThread of
       Nothing, _            -> eval (AddError "eval_CreateThread" "Current board doesn't exist" next)
       _, Nothing            -> eval (AddError "eval_CreateThread" "Thread state doesn't exist" next)
       Just board, Just comp -> go st board comp

  where
  go st board comp = do

    let
      thread_request = mkThreadRequest comp.name Nothing false false Nothing Nothing []
      board_id       = board ^. _BoardPackResponse .. boardId_

    e_thread <- rd $ postThread_ByBoardId' board_id thread_request
    case e_thread of
      Left err     -> eval (AddErrorApi "eval_CreateThread::postThread_ByBoardId'" err next)
      Right thread -> do

        -- Now we need to turn this into a thread pack
        e_thread_pack <- rd $ getThreadPack' (thread ^. _ThreadResponse .. id_)
        case e_thread_pack of
            Left err          -> eval (AddErrorApi "eval_CreateThread::getThreadPack'" err next)
            Right thread_pack -> do

              let
                thread_id = thread_pack ^. _ThreadPackResponse .. threadId_

              modify (\st' -> st'{ threads = M.update (const $ Just thread_pack) thread_id st.threads })
              eval_CreateThread eval (CompCreateThread (InputCreateThread_SetName Nothing) next)
              pure next



eval_CreateThread eval (CompCreateThread (InputCreateThread_SetName m_name) next) = do
  case m_name of
       Nothing -> modify (_ { compCreateThread = Nothing }) $> next
       (Just name) -> do
         modify (_ { compCreateThread = Just { name: name } })
         pure next
