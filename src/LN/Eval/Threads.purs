module LN.Eval.Threads (
  eval_GetThreads,
  eval_GetThreadsForBoard
) where


import Halogen                         (gets, modify)
import Data.Array                      (catMaybes, zip)
import Data.Either                     (Either(..))
import Data.Map                        as M
import Optic.Core                      ((^.), (..))
import Prelude                         (bind, pure, map, ($), (<>))

import LN.Component.Types              (EvalEff)
import LN.Input.Types                  (Input(..))
import LN.State.PageInfo               (runPageInfo)
import LN.Api                          (getThreadPacks_ByBoardId, rd, getThreadsCount_ByBoardId')
import LN.T



eval_GetThreads :: EvalEff
eval_GetThreads eval (GetThreads next) = do

  pure next

{- TODO FIXME: do we use this?
  ethreadPacks <- rd $ getThreadPacks'
  case ethreadPacks of
    Left err -> pure next
    Right (ThreadPackResponses threadPacks) -> do
      modify (_{ threads = threadPacks.threadPackResponses })
      pure next
-}



eval_GetThreadsForBoard :: EvalEff
eval_GetThreadsForBoard eval (GetThreadsForBoard board_id next) = do

  page_info <- gets _.threadsPageInfo

  ecount <- rd $ getThreadsCount_ByBoardId' board_id
  case ecount of
    Left err -> pure next
    Right counts -> do

      let new_page_info = runPageInfo counts page_info

      modify (_ { threadsPageInfo = new_page_info.pageInfo })

      ethread_packs <- rd $ getThreadPacks_ByBoardId new_page_info.params board_id
      case ethread_packs of
        Left err -> pure next
        Right (ThreadPackResponses thread_packs) -> do

          let
            users =
              (catMaybes $ map (\(ThreadPackResponse pack) -> pack.latestThreadPostUser) thread_packs.threadPackResponses)
              <>
              (map (\(ThreadPackResponse pack) -> pack.user) thread_packs.threadPackResponses)

            threads = thread_packs.threadPackResponses
            threads_map = M.fromFoldable $ zip (map (\(ThreadPackResponse pack) -> pack.threadId) threads) threads

          eval (GetUsers_MergeMap_ByUser users next)

          modify (_{ threads = threads_map })
          pure next
