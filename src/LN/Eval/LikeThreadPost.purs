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
import Optic.Core                  ((^.), (..), (.~))
import Prelude                     (bind, pure, show, return, ($), (<>))
import Purescript.Api.Helpers

import LN.Api                      (rd, postThreadPostLike_ByThreadPostId', putThreadPostLike'
                                   , getThreadPostStat')
import LN.Component.Types          (EvalEff, EvalEffP, LNEff)
import LN.Input.LikeThreadPost     (InputLikeThreadPost(..))
import LN.Input.Types              (Input(..))
import LN.State.Lens
import LN.T



eval_LikeThreadPost :: EvalEff



eval_LikeThreadPost eval (CompLikeThreadPost (InputLikeThreadPost_Like pack) next) = do
  let tplr = mkThreadPostLikeRequest Like Nothing
  packmap <- gets _.threadPosts
  newmap' <- liftAff' $ boom pack tplr packmap
  case newmap' of
       Left err     -> pure next
       Right newmap -> do
         modify (_{ threadPosts = newmap })
         pure next



eval_LikeThreadPost eval (CompLikeThreadPost (InputLikeThreadPost_Neutral pack) next) = do
  let tplr = mkThreadPostLikeRequest Neutral Nothing
  packmap <- gets _.threadPosts
  newmap' <- liftAff' $ boom pack tplr packmap
  case newmap' of
       Left err     -> pure next
       Right newmap -> do
         modify (_{ threadPosts = newmap })
         pure next
  pure next



eval_LikeThreadPost eval (CompLikeThreadPost (InputLikeThreadPost_Dislike pack) next) = do
  let tplr = mkThreadPostLikeRequest Dislike Nothing
  packmap <- gets _.threadPosts
  newmap' <- liftAff' $ boom pack tplr packmap
  case newmap' of
       Left err     -> pure next
       Right newmap -> do
         modify (_{ threadPosts = newmap })
         pure next
  pure next



eval_LikeThreadPost eval (CompLikeThreadPost (InputLikeThreadPost_Star pack) next) = do
  pure next



boom :: forall eff. ThreadPostPackResponse -> ThreadPostLikeRequest -> M.Map Int ThreadPostPackResponse -> LNEff eff (Either ApiError (M.Map Int ThreadPostPackResponse))
boom pack tplr packmap = do
  -- If mlike is Nothing, then we are creating a new "like".
  -- Otherwise, update an existing like
  etplr <- (case (pack ^. _ThreadPostPackResponse .. like_) of
       Nothing     -> rD $ postThreadPostLike_ByThreadPostId' thread_post_id tplr
       (Just like) -> rD $ putThreadPostLike' (like ^. _ThreadPostLikeResponse .. id_) tplr)
  case etplr of
       Left err -> pure $ Left err
       Right resp -> do
         estat <- rD $ getThreadPostStat' thread_post_id
         case estat of
           Left err   -> pure $ Left err
           Right stat -> do
             -- Need to update stats AND our like
             let
               new_pack  = _ThreadPostPackResponse .. like_ .~ (Just resp) $ pack
               new_pack' = _ThreadPostPackResponse .. stat_ .~ stat $ new_pack
             pure $ Right $ M.update (\_ -> Just new_pack') thread_post_id packmap
  where
  thread_post_id = pack ^. _ThreadPostPackResponse .. threadPost_ ^. _ThreadPostResponse .. id_
