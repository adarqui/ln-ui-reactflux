module LN.Eval.Like (
  eval_Like
) where



import Data.Array                  (cons)
import Data.Ebyam                  (ebyam)
import Data.Map                    as M
import Data.Maybe                  (Maybe(..), maybe)
import Data.Either                 (Either(..))
import Halogen                     (get, modify)
import Optic.Core                  ((^.), (..), (.~))
import Prelude                     (bind, pure, void, const, ($))
import Purescript.Api.Helpers

import LN.Api                      ( putLike'
                                   , deleteLike'
                                   , postLike_ByThreadPostId'
                                   , postLike_ByLeuronId'
                                   , getLikeStat
                                   , getThreadPostStat'
                                   )
import LN.Component.Types          (EvalEff, LNEff)
import LN.Ent                      (createByParamFromEnt, createResyncFromEnt)
import LN.Input.Like               (InputLike(..))
import LN.Input.Types              (Input(..))
import LN.State.Types              (State)
import LN.T                        ( Ent(..)
                                   , LikeResponse(..), _LikeResponse
                                   , LikeRequest(..)
                                   , LikeOpt(..)
                                   , id_, stat_, _ThreadPostResponse, threadPost_, like_
                                   , mkLikeRequest
                                   , ThreadPostPackResponse(..))


import Control.Monad.Aff.Free (fromAff)



eval_Like :: EvalEff




eval_Like eval (CompLike (InputLike_Un ent ent_id m_like) next) = do
  ebyam m_like (pure next) $ \like -> do
    lr <- fromAff $ boomUnLike like ent ent_id next
    case lr of
         Left err -> pure next
         Right cmd -> do
           eval cmd
           pure next



eval_Like eval (CompLike (InputLike_Like ent ent_id m_like) next) = do
  let like_req = mkLikeRequest Like Nothing 0
  st <- get
  lr <- fromAff $ boomLike m_like ent ent_id like_req next
  case lr of
       Left err -> pure next
       Right cmd -> do
         eval cmd
         pure next



eval_Like eval (CompLike (InputLike_Neutral ent ent_id m_like) next) = do
  let like_req = mkLikeRequest Neutral Nothing 0
  st <- get
  lr <- fromAff $ boomLike m_like ent ent_id like_req next
  case lr of
       Left err -> pure next
       Right cmd -> do
         eval cmd
         pure next



eval_Like eval (CompLike (InputLike_Dislike ent ent_id m_like) next) = do
  let like_req = mkLikeRequest Dislike Nothing 0
  st <- get
  lr <- fromAff $ boomLike m_like ent ent_id like_req next
  case lr of
       Left err -> pure next
       Right cmd -> do
         eval cmd
         pure next



postLike ent ent_id like_req =
  case ent of
       Ent_ThreadPost -> postLike_ByThreadPostId' ent_id like_req
       Ent_Leuron     -> postLike_ByLeuronId' ent_id like_req



updateLike ent ent_id like like_stat st =
  case ent of
       Ent_ThreadPost -> st{ threadPosts = M.update (\(ThreadPostPackResponse pack) -> Just $ ThreadPostPackResponse pack{ like = Just like }) ent_id st.threadPosts }
       _              -> st




boomLike
  :: forall a eff.
     Maybe LikeResponse
  -> Ent
  -> Int
  -> LikeRequest
  -> a
  -> LNEff eff (Either ApiError (Input a))
boomLike m_like ent ent_id like_req next = do

  -- If m_like is Nothing, then we are creating a new "like".
  -- Otherwise, update an existing like
  e_like_req <- (case m_like of
       Nothing     -> rD $ postLike ent ent_id like_req
       (Just like) -> rD $ putLike' (like ^. _LikeResponse .. id_) like_req)

  case e_like_req of
       Left err   -> pure $ Left err
       Right resp -> do
         -- Need to update stats AND our like
         pure $ Right $ createResyncFromEnt ent ent_id next

  where
  by_params = maybe [] (`cons` []) $ createByParamFromEnt ent ent_id



boomUnLike
  :: forall a eff.
     LikeResponse
  -> Ent
  -> Int
  -> a
  -> LNEff eff (Either ApiError (Input a))
boomUnLike like ent ent_id next = do

   e_resp <- rD $ deleteLike' (like ^. _LikeResponse .. id_)
   case e_resp of
     Left err   -> pure $ Left err
     Right _ -> do
       -- Need to update stats AND our like
       pure $ Right $ createResyncFromEnt ent ent_id next
