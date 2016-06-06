module LN.Eval.ThreadPosts (
  eval_GetThreadPosts,
  eval_GetThreadPost,
  eval_GetThreadPostsForThread,
  eval_ThreadPost
) where



import Halogen               (gets, modify)
import Data.Either           (Either(..))
import Data.Map              as M
import Optic.Core            ((^.), (..))
import Prelude               (bind, pure, map, ($))
import LN.Component.Types    (EvalEff)
import LN.Input.ThreadPost   (InputThreadPost(..), ThreadPost_Mod(..))
import LN.Input.Types        (Input(..))
import LN.Helpers.Map        (mergeMapArray)
import LN.State.PageInfo     (runPageInfo)
import LN.Api.Internal       (getThreadPostsCount_ByThreadId' , getThreadPostPacks_ByThreadId)
import LN.Api.Helpers        (rd)
import LN.T



eval_GetThreadPosts :: EvalEff
eval_GetThreadPosts eval (GetThreadPosts next) = pure next



eval_GetThreadPost :: EvalEff
eval_GetThreadPost eval (GetThreadPost thread_post_id next) = pure next



eval_GetThreadPostsForThread :: EvalEff
eval_GetThreadPostsForThread eval (GetThreadPostsForThread thread_id next) = do

  modify (_{ threadPosts = (M.empty :: M.Map Int ThreadPostPackResponse) })

  page_info <- gets _.threadPostsPageInfo

  e_count <- rd $ getThreadPostsCount_ByThreadId' thread_id
  case e_count of
    Left err     -> eval (AddErrorApi "eval_GetThreadPostsForThread::getThreadPostsCount_ByThreadId'" err next)
    Right counts -> do

      let new_page_info = runPageInfo counts page_info

      modify (_ { threadPostsPageInfo = new_page_info.pageInfo })

      e_posts <- rd $ getThreadPostPacks_ByThreadId new_page_info.params thread_id
      case e_posts of
        Left err -> eval (AddErrorApi "eval_GetThreadPostsForThread::getThreadPostPacks_ByThreadId" err next)
        Right (ThreadPostPackResponses posts) -> do

          let
            users_ids =
              map (\thread_post_pack -> thread_post_pack ^. _ThreadPostPackResponse .. threadPost_ ^. _ThreadPostResponse .. userId_) posts.threadPostPackResponses

          eval (GetUsers_MergeMap_ByUserId users_ids next)

          modify (\st -> st{
                 threadPosts =
                   mergeMapArray
                     st.threadPosts
                     posts.threadPostPackResponses
                     (\x -> x ^. _ThreadPostPackResponse .. threadPost_ ^. _ThreadPostResponse .. id_)
                 })
          pure next



eval_ThreadPost :: EvalEff
eval_ThreadPost eval (CompThreadPost sub next) = do
  case sub of
    InputThreadPost_Mod q -> do
      case q of
        _ -> pure next
