module LN.Eval.ThreadPost (
  eval_ThreadPost
) where



import Data.Either          (Either(..))
import Data.Functor         (($>))
import Data.Map             as M
import Data.Maybe           (Maybe(..), maybe)
import Halogen              (get, modify)
import Optic.Core           ((^.), (..))
import Prelude              (bind, pure, ($))

import LN.Api               (rd, postThreadPost_ByThreadId', getThreadPostPack')
import LN.Component.Types   (EvalEff)
import LN.Input.ThreadPost  (InputThreadPost(..))
import LN.Input.Types       (Input(..))
import LN.T                 (_ThreadPackResponse, ThreadPostRequest (..)
                            , ThreadPostResponse (..), _ThreadResponse, thread_, id_
                            , PostData (..))



eval_ThreadPost :: EvalEff



eval_ThreadPost eval (CompThreadPost InputThreadPost_Nop next) = do
  pure next



eval_ThreadPost eval (CompThreadPost InputThreadPost_Post next) = do
  st <- get
  let thread_id = maybe 0 (\pack -> pack ^. _ThreadPackResponse .. thread_ ^. _ThreadResponse .. id_) st.currentThread
  let mthread_post_request = st.currentThreadPost

  case mthread_post_request of
    Nothing                  -> pure next
    Just thread_post_request -> do

      epost <- rd $ postThreadPost_ByThreadId' thread_id thread_post_request
      case epost of
        Left err   -> pure next
        Right (ThreadPostResponse post) -> do

          epack <- rd $ getThreadPostPack' post.id
          case epack of
            Left err   -> pure next
            Right pack -> do
              modify (\st' -> st'{ threadPosts = M.insert post.id pack st.threadPosts })
              pure next




eval_ThreadPost eval (CompThreadPost (InputThreadPost_SetBody mbody) next) = do
  case mbody of
       Nothing -> modify (_ { currentThreadPost = Nothing }) $> next
       (Just body) -> do
         -- TODO FIXME: tags
         modify (_ { currentThreadPost = Just $ ThreadPostRequest { title: Nothing, body: PostDataBBCode body, tags: [], privateTags: [] } })
         pure next
