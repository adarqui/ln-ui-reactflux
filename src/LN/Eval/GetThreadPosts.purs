module LN.Eval.GetThreadPosts (
  eval_GetThreadPosts,
  eval_GetThreadPost,
  eval_GetThreadPostsForThread
) where



import Halogen               (gets, modify)
import Data.Array            (head)
import Data.Either           (Either(..))
import Data.Maybe            (maybe)
import Optic.Core            ((^.), (..))
import Prelude               (bind, pure, map, ($), (-), (*), (/), (+))
import LN.Component.Types    (EvalEff)
import LN.Input.Types        (Input(..))
import LN.State.Helpers      (mergeMapArray)
import LN.Api.Internal       (getThreadPosts_ByThreadId, getThreadPostsCount_ByThreadId'
                             , getThreadPostPacks_ByThreadId, getThreadPosts')
import LN.Api.Helpers        (rd)
import LN.T



eval_GetThreadPosts :: EvalEff
eval_GetThreadPosts eval (GetThreadPosts next) = do
  pure next



eval_GetThreadPost :: EvalEff
eval_GetThreadPost eval (GetThreadPost thread_post_id next) = do
  pure next



eval_GetThreadPostsForThread :: EvalEff
eval_GetThreadPostsForThread eval (GetThreadPostsForThread thread_id next) = do

  pageInfo <- gets _.threadPostsPageInfo

  ecount <- rd $ getThreadPostsCount_ByThreadId' thread_id
  case ecount of
    Left err -> pure next
    Right (CountResponses counts) -> do

      let count = maybe 0 (\(CountResponse count) -> count.n) (head counts.countResponses)

      modify (_ { threadPostsPageInfo = pageInfo { totalResults = count, totalPages = (count / pageInfo.resultsPerPage)+1 } })

      eposts <- rd $ getThreadPostPacks_ByThreadId [Limit pageInfo.resultsPerPage, Offset ((pageInfo.currentPage-1) * pageInfo.resultsPerPage), SortOrder pageInfo.sortOrder] thread_id
      case eposts of
        Left err -> pure next
        Right (ThreadPostPackResponses posts) -> do

          let
            users_ids =
              map (\thread_post_pack -> thread_post_pack ^. _ThreadPostPackResponse .. threadPost_ ^. _ThreadPostResponse .. userId_) posts.threadPostPackResponses

          eval (GetUsers_MergeMap_ByUserId users_ids next)

--          modify (\st -> st{ threadPosts = M.union st.threadPosts (posts.threadPostPackResponses })
          modify (\st -> st{
                 threadPosts =
                   mergeMapArray
                     st.threadPosts
                     posts.threadPostPackResponses
                     (\x -> x ^. _ThreadPostPackResponse .. threadPost_ ^. _ThreadPostResponse .. id_)
                 })
          pure next
