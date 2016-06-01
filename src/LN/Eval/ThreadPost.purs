module LN.Eval.ThreadPost (
  eval_ThreadPost
) where



import Data.Either          (Either(..))
import Data.Functor         (($>))
import Data.Map             as M
import Data.Maybe           (Maybe(..))
import Halogen              (get, modify)
import Prelude              (bind, pure, ($))

import LN.Api               (rd, postThreadPost_ByThreadId', getThreadPostPack')
import LN.Component.Types   (EvalEff)
import LN.Input.ThreadPost  (InputThreadPost(..))
import LN.Input.Types       (Input(..))
import LN.T                 ( ThreadPackResponse(..)
                            , ThreadPostRequest(..)
                            , ThreadPostResponse(..)
                            , PostData(..))



eval_ThreadPost :: EvalEff
eval_ThreadPost eval (CompThreadPost InputThreadPost_Nop next) = pure next
eval_ThreadPost eval (CompThreadPost InputThreadPost_Post next) = pure next
eval_ThreadPost eval (CompThreadPost (InputThreadPost_SetBody m_body) next) = pure next


{-
TODO FIXME: Move this to the new ThreadPost_Mod style

eval_ThreadPost eval (CompThreadPost InputThreadPost_Nop next) = do
  pure next



eval_ThreadPost eval (CompThreadPost InputThreadPost_Post next) = do
  st <- get
  case st.currentThread, st.currentThreadPost of
       Nothing, _                    -> eval (AddError "eval_ThreadPost(Post)" "Thread doesn't exist" next)
       _, Nothing                    -> eval (AddError "eval_ThreadPost(Post)" "currentThreadPost doesn't exist" next)
       Just thread, Just thread_post -> go st thread thread_post

  where
  go st (ThreadPackResponse thread) thread_post_request = do
      e_post <- rd $ postThreadPost_ByThreadId' thread.threadId thread_post_request
      case e_post of
        Left err                        -> eval (AddErrorApi "eval_ThreadPost(Post)::postThreadPost_ByThreadId'" err next)
        Right (ThreadPostResponse post) -> do

          e_pack <- rd $ getThreadPostPack' post.id
          case e_pack of
            Left err   -> eval (AddErrorApi "eval_ThreadPost(Post)::getThreadPostPack'" err next)
            Right pack -> do
              modify (\st' -> st'{ threadPosts = M.insert post.id pack st.threadPosts })
              pure next




eval_ThreadPost eval (CompThreadPost (InputThreadPost_SetBody m_body) next) = do
  case m_body of
       Nothing     -> modify (_ { currentThreadPost = Nothing }) $> next
       (Just body) -> do
         -- TODO FIXME: tags
         modify (_ { currentThreadPost = Just $ ThreadPostRequest { title: Nothing, body: PostDataBBCode body, tags: [], privateTags: [] } })
         pure next
         -}
