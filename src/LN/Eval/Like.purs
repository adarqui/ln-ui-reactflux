module LN.Eval.Like (
  eval_Like
) where



import Data.Map                    as M
import Data.Maybe                  (Maybe(..))
import Data.Either                 (Either(..))
import Halogen                     (get, gets, modify, liftAff')
import Optic.Core                  ((^.), (..), (.~))
import Prelude                     (bind, pure, void, const, ($))
import Purescript.Api.Helpers

import LN.Api                      ( putLike'
                                   , postStar_ByThreadPostId', deleteStar'
                                   , postLike_ByThreadPostId'
                                   , postLike_ByLeuronId'
                                   , getLikeStat'
                                   , getThreadPostStat'
                                   )
import LN.Component.Types          (EvalEff, LNEff)
import LN.Input.Like               (InputLike(..))
import LN.Input.Types              (Input(..))
import LN.State.Types              (State)
import LN.T                        ( Ent(..)
                                   , LikeResponse(..), _LikeResponse
                                   , LikeRequest(..)
                                   , LikeOpt(..)
                                   , id_, _StarResponse, stat_, _ThreadPostResponse, threadPost_, like_
                                   , mkStarRequest, mkLikeRequest)



eval_Like :: EvalEff



eval_Like eval (CompLike (InputLike_Like ent ent_id mlike) next) = do
  let like_req = mkLikeRequest Like Nothing
  st <- get
  lr <- liftAff' $ boomLike st mlike ent ent_id like_req
  case lr of
       Left err -> pure next
       Right st' -> do
         modify (const st')
         pure next



postLike ent ent_id like_req =
  case ent of
       Ent_ThreadPost -> postLike_ByThreadPostId' ent_id like_req
       Ent_Leuron     -> postLike_ByLeuronId' ent_id like_req



boomLike
  :: forall eff.
     State
  -> Maybe LikeResponse
  -> Ent
  -> Int
  -> LikeRequest
  -> LNEff eff (Either ApiError State)
boomLike st mlike ent ent_id like_req = do

  -- If mlike is Nothing, then we are creating a new "like".
  -- Otherwise, update an existing like
  elike_req <- (case mlike of
       Nothing     -> rD $ postLike ent ent_id like_req
       (Just like) -> rD $ putLike' (like ^. _LikeResponse .. id_) like_req)

  case elike_req of
       Left err -> pure $ Left err
       Right resp -> do
         let like_id = resp ^. _LikeResponse .. id_
         estat <- rD $ getLikeStat' like_id
         case estat of
           Left err   -> pure $ Left err
           Right stat -> do
             -- Need to update stats AND our like
             pure $ Right $ st



{-
eval_Like eval (CompLike (InputLike_Neutral ent ent_id mlike) next) = do
  let like_req = mkLikeRequest Neutral Nothing
  packmap <- gets _.threadPosts
  newmap' <- liftAff' $ boomLike pack like_req packmap
  case newmap' of
       Left err     -> pure next
       Right newmap -> do
         modify (_{ threadPosts = newmap })
         pure next



eval_Like eval (CompLike (InputLike_Dislike ent ent_id mlike) next) = do
  let like_req = mkLikeRequest Dislike Nothing
  packmap <- gets _.threadPosts
  newmap' <- liftAff' $ boomLike pack like_req packmap
  case newmap' of
       Left err     -> pure next
       Right newmap -> do
         modify (_{ threadPosts = newmap })
         pure next



eval_Like eval (CompLike (InputLike_Star ent ent_id mstar) next) = do
  let star_req = mkStarRequest Nothing
  packmap <- gets _.threadPosts
  newmap' <- liftAff' $ boomStar pack star_req packmap
  case newmap' of
       Left err     -> pure next
       Right newmap -> do
         modify (_{ threadPosts = newmap })
         pure next



-}

{-
boomLike :: forall eff. ThreadPostPackResponse -> LikeRequest -> M.Map Int ThreadPostPackResponse -> LNEff eff (Either ApiError (M.Map Int ThreadPostPackResponse))
boomLike pack like_req packmap = do
  -- If mlike is Nothing, then we are creating a new "like".
  -- Otherwise, update an existing like
  elike_req <- (case (pack ^. _ThreadPostPackResponse .. like_) of
       Nothing     -> rD $ postLike_ByThreadPostId' thread_post_id like_req
       (Just like) -> rD $ putLike' (like ^. _LikeResponse .. id_) like_req)
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
  -}



{-
boomStar :: forall eff. ThreadPostPackResponse -> StarRequest -> M.Map Int ThreadPostPackResponse -> LNEff eff (Either ApiError (M.Map Int ThreadPostPackResponse))
boomStar pack star_req packmap = do
  case (pack ^. _ThreadPostPackResponse .. star_) of

    -- If mstar is Nothing, then we are creating a new "star".
    Nothing     -> do
      estar_req <- rD $ postStar_ByThreadPostId' thread_post_id star_req
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
      void $ rD $ deleteStar' (star ^. _StarResponse .. id_)
      let new_pack  = _ThreadPostPackResponse .. star_ .~ Nothing $ pack
      pure $ Right $ M.update (\_ -> Just new_pack) thread_post_id packmap

  where
  thread_post_id = pack ^. _ThreadPostPackResponse .. threadPost_ ^. _ThreadPostResponse .. id_
  -}
