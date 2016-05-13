module LN.Eval.LikeThreadPost (
  eval_LikeThreadPost
) where



import Control.Monad.Aff           (Aff())
import Control.Monad.Aff.Class     (liftAff)
import Data.Map                    as M
import Data.Maybe                  (Maybe(..), maybe)
import Data.Either                 (Either(..))
import Data.Functor                (($>))
import Halogen                     (get, gets, modify, liftAff')
import Optic.Core                  ((^.), (..))
import Prelude                     (bind, pure, show, return, ($), (<>))
import Purescript.Api.Helpers

import LN.Api                      (rd, postThreadPostLike_ByThreadPostId', putThreadPostLike')
import LN.Component.Types          (EvalEff, EvalEffP, LNEff)
import LN.Input.LikeThreadPost     (InputLikeThreadPost(..))
import LN.Input.Types              (Input(..))
import LN.State.Lens
import LN.T



eval_LikeThreadPost :: EvalEff



eval_LikeThreadPost eval (CompLikeThreadPost (InputLikeThreadPost_Like thread_post_id mlike_resp) next) = do
  let tplr = mkThreadPostLikeRequest Like Nothing
  packmap <- gets _.threadPosts
  newmap' <- liftAff' $ boom thread_post_id mlike_resp tplr packmap
  case newmap' of
       Left err     -> pure next
       Right newmap -> do
         modify (_{ threadPosts = newmap })
         pure next



eval_LikeThreadPost eval (CompLikeThreadPost (InputLikeThreadPost_Neutral thread_post_id mlike_resp) next) = do
  let tplr = mkThreadPostLikeRequest Neutral Nothing
  packmap <- gets _.threadPosts
  newmap' <- liftAff' $ boom thread_post_id mlike_resp tplr packmap
  case newmap' of
       Left err     -> pure next
       Right newmap -> do
         modify (_{ threadPosts = newmap })
         pure next
  pure next



eval_LikeThreadPost eval (CompLikeThreadPost (InputLikeThreadPost_Dislike thread_post_id mlike_resp) next) = do
  let tplr = mkThreadPostLikeRequest Dislike Nothing
  packmap <- gets _.threadPosts
  newmap' <- liftAff' $ boom thread_post_id mlike_resp tplr packmap
  case newmap' of
       Left err     -> pure next
       Right newmap -> do
         modify (_{ threadPosts = newmap })
         pure next
  pure next



eval_LikeThreadPost eval (CompLikeThreadPost (InputLikeThreadPost_Star thread_post_id) next) = do
  pure next



boom :: forall eff. Int -> Maybe ThreadPostLikeResponse -> ThreadPostLikeRequest -> M.Map Int ThreadPostPackResponse -> LNEff eff (Either ApiError (M.Map Int ThreadPostPackResponse))
boom thread_post_id mlike tplr packmap = do
  -- If mlike is Nothing, then we are creating a new "like".
  -- Otherwise, update an existing like
  etplr <- (case mlike of
       Nothing -> rD $ postThreadPostLike_ByThreadPostId' thread_post_id tplr
       (Just like) -> rD $ putThreadPostLike' (like ^. _ThreadPostLikeResponse .. id_) tplr)
  case etplr of
       Left err -> pure $ Left err
       Right resp -> do
         pure $ Right packmap
