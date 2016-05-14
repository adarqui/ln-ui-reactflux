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
import Prelude                     (bind, pure, show, return, void, ($), (<>))
import Purescript.Api.Helpers

import LN.Api                      (rd
                                   , postThreadPostLike_ByThreadPostId', putThreadPostLike', getThreadPostStat'
                                   , postThreadPostStar_ByThreadPostId', deleteThreadPostStar', getThreadPostStar'
                                   )
import LN.Component.Types          (EvalEff, EvalEffP, LNEff)
import LN.Input.LikeThreadPost     (InputLikeThreadPost(..))
import LN.Input.Types              (Input(..))
import LN.T



eval_LikeThreadPost :: EvalEff



eval_LikeThreadPost eval (CompLikeThreadPost (InputLikeThreadPost_Like pack) next) = do
  let tplr = mkThreadPostLikeRequest Like Nothing
  packmap <- gets _.threadPosts
  newmap' <- liftAff' $ boomLike pack tplr packmap
  case newmap' of
       Left err     -> pure next
       Right newmap -> do
         modify (_{ threadPosts = newmap })
         pure next



eval_LikeThreadPost eval (CompLikeThreadPost (InputLikeThreadPost_Neutral pack) next) = do
  let tplr = mkThreadPostLikeRequest Neutral Nothing
  packmap <- gets _.threadPosts
  newmap' <- liftAff' $ boomLike pack tplr packmap
  case newmap' of
       Left err     -> pure next
       Right newmap -> do
         modify (_{ threadPosts = newmap })
         pure next



eval_LikeThreadPost eval (CompLikeThreadPost (InputLikeThreadPost_Dislike pack) next) = do
  let tplr = mkThreadPostLikeRequest Dislike Nothing
  packmap <- gets _.threadPosts
  newmap' <- liftAff' $ boomLike pack tplr packmap
  case newmap' of
       Left err     -> pure next
       Right newmap -> do
         modify (_{ threadPosts = newmap })
         pure next



eval_LikeThreadPost eval (CompLikeThreadPost (InputLikeThreadPost_Star pack) next) = do
  let tpsr = mkThreadPostStarRequest Nothing
  packmap <- gets _.threadPosts
  newmap' <- liftAff' $ boomStar pack tpsr packmap
  case newmap' of
       Left err     -> pure next
       Right newmap -> do
         modify (_{ threadPosts = newmap })
         pure next



boomLike :: forall eff. ThreadPostPackResponse -> ThreadPostLikeRequest -> M.Map Int ThreadPostPackResponse -> LNEff eff (Either ApiError (M.Map Int ThreadPostPackResponse))
boomLike pack tplr packmap = do
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



boomStar :: forall eff. ThreadPostPackResponse -> ThreadPostStarRequest -> M.Map Int ThreadPostPackResponse -> LNEff eff (Either ApiError (M.Map Int ThreadPostPackResponse))
boomStar pack tplr packmap = do
  case (pack ^. _ThreadPostPackResponse .. star_) of

    -- If mstar is Nothing, then we are creating a new "star".
    Nothing     -> do
      etplr <- rD $ postThreadPostStar_ByThreadPostId' thread_post_id tplr
      case etplr of
           Left err -> pure $ Left err
           Right resp -> do
             estat <- rD $ getThreadPostStat' thread_post_id
             case estat of
               Left err   -> pure $ Left err
               Right stat -> do
                 -- Need to update stats AND our star
                 let
                   new_pack  = _ThreadPostPackResponse .. star_ .~ (Just resp) $ pack
                   new_pack' = _ThreadPostPackResponse .. stat_ .~ stat $ new_pack
                 pure $ Right $ M.update (\_ -> Just new_pack') thread_post_id packmap

    (Just star) -> do
    -- Otherwise, remove the star .. ie, a toggle
      void $ rD $ deleteThreadPostStar' (star ^. _ThreadPostStarResponse .. id_)
      let new_pack  = _ThreadPostPackResponse .. star_ .~ Nothing $ pack
      pure $ Right $ M.update (\_ -> Just new_pack) thread_post_id packmap

  where
  thread_post_id = pack ^. _ThreadPostPackResponse .. threadPost_ ^. _ThreadPostResponse .. id_
