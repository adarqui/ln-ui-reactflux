module LN.Eval.Threads (
  eval_GetThreads,
  eval_GetThreadsForBoard,
  eval_Thread
) where



import Halogen                         (gets, modify)
import Data.Array                      (catMaybes)
import Data.Either                     (Either(..))
import Data.Map                        as M
import Prelude                         (bind, pure, map, ($), (<>))

import LN.Component.Types              (EvalEff)
import LN.Helpers.Map                  (idmapFrom)
import LN.Input.Thread                 (InputThread(..), Thread_Mod(..))
import LN.Input.Types                  (Input(..))
import LN.State.PageInfo               (runPageInfo)
import LN.Api                          (getThreadPacks_ByBoardId, rd, getThreadsCount_ByBoardId')
import LN.T



eval_GetThreads :: EvalEff
eval_GetThreads eval (GetThreads next) = pure next



eval_GetThreadsForBoard :: EvalEff
eval_GetThreadsForBoard eval (GetThreadsForBoard board_id next) = do

  modify (_{ threads = (M.empty :: M.Map Int ThreadPackResponse) })

  page_info <- gets _.threadsPageInfo

  e_count <- rd $ getThreadsCount_ByBoardId' board_id
  case e_count of
    Left err     -> eval (AddErrorApi "eval_GetThreadsForBoard::getThreadsCount_ByBoardId'" err next)
    Right counts -> do

      let new_page_info = runPageInfo counts page_info

      modify (_ { threadsPageInfo = new_page_info.pageInfo })

      e_thread_packs <- rd $ getThreadPacks_ByBoardId new_page_info.params board_id
      case e_thread_packs of
        Left err -> pure next
        Right (ThreadPackResponses thread_packs) -> do

          let
            users =
              (catMaybes $ map (\(ThreadPackResponse pack) -> pack.latestThreadPostUser) thread_packs.threadPackResponses)
              <>
              (map (\(ThreadPackResponse pack) -> pack.user) thread_packs.threadPackResponses)

            threads_map = idmapFrom (\(ThreadPackResponse p) -> p.threadId) thread_packs.threadPackResponses

          eval (GetUsers_MergeMap_ByUser users next)

          modify (_{ threads = threads_map })
          pure next



eval_Thread :: EvalEff
eval_Thread eval (CompThread sub next) = do
  case sub of
    InputThread_Mod q -> do
      case q of
        _ -> pure next
