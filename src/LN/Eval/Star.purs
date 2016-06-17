module LN.Eval.Star (
  eval_Star
) where



import Data.Maybe                  (Maybe(..))
import Data.Either                 (Either(..))
import Halogen                     (get, modify)
import Optic.Core                  ((^.), (..), (.~))
import Prelude                     (bind, pure, void, const, ($))
import Purescript.Api.Helpers

import LN.Api                      ( putStar'
                                   , postStar_ByThreadPostId', postStar_ByLeuronId'
                                   , deleteStar'
                                   , getStarStat'
                                   , getThreadPostStat'
                                   )
import LN.Component.Types          (EvalEff, LNEff)
import LN.Input.Star               (InputStar(..))
import LN.Input.Types              (Input(..))
import LN.State.Types              (State)
import LN.T                        ( Ent(..)
                                   , StarResponse(..), _StarResponse
                                   , StarRequest(..)
                                   , id_, stat_, _ThreadPostResponse, threadPost_, like_
                                   , mkStarRequest, mkStarRequest)

import Control.Monad.Aff.Free (fromAff)


eval_Star :: EvalEff



eval_Star eval (CompStar (InputStar ent ent_id mstar) next) = do
  let star_req = mkStarRequest Nothing 0
  st <- get
  lr <- fromAff $ boomStar st mstar ent ent_id star_req
  case lr of
       Left err -> pure next
       Right st' -> do
         modify (const st')
         pure next



postStar ent ent_id star_req =
  case ent of
       Ent_ThreadPost -> postStar_ByThreadPostId' ent_id star_req
       Ent_Leuron     -> postStar_ByLeuronId' ent_id star_req



boomStar
  :: forall eff.
     State
  -> Maybe StarResponse
  -> Ent
  -> Int
  -> StarRequest
  -> LNEff eff (Either ApiError State)
boomStar st m_star ent ent_id star_req = do

  -- If m_star is Nothing, then we are creating a new "star".
  -- Otherwise, update an existing star
  e_star_req <- (case m_star of
       Nothing     -> rD $ postStar ent ent_id star_req
       (Just star) -> rD $ putStar' (star ^. _StarResponse .. id_) star_req)

  case e_star_req of
       Left err   -> pure $ Left err
       Right resp -> do
         let star_id = resp ^. _StarResponse .. id_
         e_stat <- rD $ getStarStat' star_id
         case e_stat of
           Left err   -> pure $ Left err
           Right stat -> do
             -- Need to update stats AND our star
             pure $ Right $ st



{-
eval_Like eval (CompLike (InputLike_Neutral ent ent_id mlike) next) = do
  let like_req = mkLikeRequest Neutral Nothing
  packmap <- gets _.threadPosts
  newmap' <- fromAff $ boomLike pack like_req packmap
  case newmap' of
       Left err     -> pure next
       Right newmap -> do
         modify (_{ threadPosts = newmap })
         pure next



eval_Like eval (CompLike (InputLike_Dislike ent ent_id mlike) next) = do
  let like_req = mkLikeRequest Dislike Nothing
  packmap <- gets _.threadPosts
  newmap' <- fromAff $ boomLike pack like_req packmap
  case newmap' of
       Left err     -> pure next
       Right newmap -> do
         modify (_{ threadPosts = newmap })
         pure next



eval_Like eval (CompLike (InputLike_Star ent ent_id mstar) next) = do
  let star_req = mkStarRequest Nothing
  packmap <- gets _.threadPosts
  newmap' <- fromAff $ boomStar pack star_req packmap
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
