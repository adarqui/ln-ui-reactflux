module LN.Eval.LikeThreadPost (
  eval_LikeThreadPost
) where



import Data.Map                    as M
import Data.Maybe                  (Maybe(..))
import Data.Either                 (Either(..))
import Halogen                     (gets, modify, liftAff')
import Optic.Core                  ((^.), (..), (.~))
import Prelude                     (bind, pure, void, ($))
import Purescript.Api.Helpers

import LN.Api                      ( postThreadPostLike_ByThreadPostId', putThreadPostLike', getThreadPostStat'
                                   , postThreadPostStar_ByThreadPostId', deleteThreadPostStar'
                                   )
import LN.Component.Types          (EvalEff, LNEff)
import LN.Input.LikeThreadPost     (InputLikeThreadPost(..))
import LN.Input.Types              (Input(..))
import LN.T                        (ThreadPostPackResponse, StarRequest, LikeRequest
                                   , LikeOpt(Dislike, Neutral, Like), star_, _ThreadPostPackResponse
                                   , id_, _StarResponse, stat_, _ThreadPostResponse, threadPost_, like_
                                   , _LikeResponse, mkStarRequest, mkLikeRequest)



eval_LikeThreadPost :: EvalEff



eval_LikeThreadPost eval (CompLikeThreadPost (InputLikeThreadPost_Like pack) next) = do
  let like_req = mkLikeRequest Like Nothing
  packmap <- gets _.threadPosts
  newmap' <- liftAff' $ boomLike pack like_req packmap
  case newmap' of
       Left err     -> pure next
       Right newmap -> do
         modify (_{ threadPosts = newmap })
         pure next



eval_LikeThreadPost eval (CompLikeThreadPost (InputLikeThreadPost_Neutral pack) next) = do
  let like_req = mkLikeRequest Neutral Nothing
  packmap <- gets _.threadPosts
  newmap' <- liftAff' $ boomLike pack like_req packmap
  case newmap' of
       Left err     -> pure next
       Right newmap -> do
         modify (_{ threadPosts = newmap })
         pure next



eval_LikeThreadPost eval (CompLikeThreadPost (InputLikeThreadPost_Dislike pack) next) = do
  let like_req = mkLikeRequest Dislike Nothing
  packmap <- gets _.threadPosts
  newmap' <- liftAff' $ boomLike pack like_req packmap
  case newmap' of
       Left err     -> pure next
       Right newmap -> do
         modify (_{ threadPosts = newmap })
         pure next



eval_LikeThreadPost eval (CompLikeThreadPost (InputLikeThreadPost_Star pack) next) = do
  let star_req = mkStarRequest Nothing
  packmap <- gets _.threadPosts
  newmap' <- liftAff' $ boomStar pack star_req packmap
  case newmap' of
       Left err     -> pure next
       Right newmap -> do
         modify (_{ threadPosts = newmap })
         pure next



boomLike :: forall eff. ThreadPostPackResponse -> LikeRequest -> M.Map Int ThreadPostPackResponse -> LNEff eff (Either ApiError (M.Map Int ThreadPostPackResponse))
boomLike pack like_req packmap = do
  -- If mlike is Nothing, then we are creating a new "like".
  -- Otherwise, update an existing like
  elike_req <- (case (pack ^. _ThreadPostPackResponse .. like_) of
       Nothing     -> rD $ postThreadPostLike_ByThreadPostId' thread_post_id like_req
       (Just like) -> rD $ putThreadPostLike' (like ^. _LikeResponse .. id_) like_req)
  case elike_req of
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



boomStar :: forall eff. ThreadPostPackResponse -> StarRequest -> M.Map Int ThreadPostPackResponse -> LNEff eff (Either ApiError (M.Map Int ThreadPostPackResponse))
boomStar pack star_req packmap = do
  case (pack ^. _ThreadPostPackResponse .. star_) of

    -- If mstar is Nothing, then we are creating a new "star".
    Nothing     -> do
      estar_req <- rD $ postThreadPostStar_ByThreadPostId' thread_post_id star_req
      case estar_req of
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
      void $ rD $ deleteThreadPostStar' (star ^. _StarResponse .. id_)
      let new_pack  = _ThreadPostPackResponse .. star_ .~ Nothing $ pack
      pure $ Right $ M.update (\_ -> Just new_pack) thread_post_id packmap

  where
  thread_post_id = pack ^. _ThreadPostPackResponse .. threadPost_ ^. _ThreadPostResponse .. id_
