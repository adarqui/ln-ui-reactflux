module LN.Eval.ThreadPost (
  eval_ThreadPost
) where



import Data.Either          (Either(..))
import Data.Functor         (($>))
import Data.Maybe           (Maybe(..), maybe)
import Data.Maybe.Unsafe    (fromJust)
import Halogen              (get, modify)
import Optic.Core           ((^.), (..))
import Prelude              (bind, pure, ($), (++))

import LN.Api               (rd, postThreadPost_ByThreadId')
import LN.Component.Types   (EvalEff)
import LN.Input.ThreadPost  (InputThreadPost(..))
import LN.Input.Types       (Input(..))
import LN.State.Lens
import LN.T
import LN.T.Pack.ThreadPost.Response (defaultThreadPostPackResponse)
import LN.T.User.Helpers    (userResponseToSanitizedResponse)



eval_ThreadPost :: EvalEff



eval_ThreadPost eval (CompThreadPost InputThreadPost_Nop next) = do
  pure next



eval_ThreadPost eval (CompThreadPost InputThreadPost_Post next) = do
  st <- get
  let thread_id = maybe 0 (\thread -> thread ^. _ThreadResponse .. id_) (st ^. stCurrentThread)
  let mthread_post_request = (st ^. stCurrentThreadPost)
  case mthread_post_request of
    Nothing                  -> pure next
    Just thread_post_request -> do
      epost <- rd $ postThreadPost_ByThreadId' thread_id thread_post_request
      case epost of
        Left err   -> pure next
        Right post -> do
          -- TODO FIXME: needs to be a thread post pack
          let
            pack = defaultThreadPostPackResponse post user
            user = userResponseToSanitizedResponse ((fromJust (st ^. stMe)) ^. _UserPackResponse .. user_)
          modify (_ { threadPosts = (st ^. stThreadPosts) ++ [pack] })
          pure next




eval_ThreadPost eval (CompThreadPost (InputThreadPost_SetBody mbody) next) = do
  case mbody of
       Nothing -> modify (_ { currentThreadPost = Nothing }) $> next
       (Just body) -> do
         -- TODO FIXME: tags
         modify (_ { currentThreadPost = Just $ ThreadPostRequest { title: Nothing, body: PostDataBBCode body, tags: [], privateTags: [] } })
         pure next
