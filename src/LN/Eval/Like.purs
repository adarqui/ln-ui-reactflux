module LN.Eval.Like (
  eval_Like
) where



import Data.Maybe                  (Maybe(..))
import Data.Either                 (Either(..))
import Halogen                     (get, modify)
import Optic.Core                  ((^.), (..), (.~))
import Prelude                     (bind, pure, void, const, ($))
import Purescript.Api.Helpers

import LN.Api                      ( putLike'
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
                                   , id_, stat_, _ThreadPostResponse, threadPost_, like_
                                   , mkLikeRequest)


import Control.Monad.Aff.Free (fromAff)



eval_Like :: EvalEff



eval_Like eval (CompLike (InputLike_Like ent ent_id mlike) next) = do
  let like_req = mkLikeRequest Like Nothing 0
  st <- get
  lr <- fromAff $ boomLike st mlike ent ent_id like_req
  case lr of
       Left err -> pure next
       Right st' -> do
         modify (const st')
         pure next



eval_Like eval (CompLike (InputLike_Neutral ent ent_id mlike) next) = do
  let like_req = mkLikeRequest Neutral Nothing 0
  st <- get
  lr <- fromAff $ boomLike st mlike ent ent_id like_req
  case lr of
       Left err -> pure next
       Right st' -> do
         modify (const st')
         pure next



eval_Like eval (CompLike (InputLike_Dislike ent ent_id mlike) next) = do
  let like_req = mkLikeRequest Dislike Nothing 0
  st <- get
  lr <- fromAff $ boomLike st mlike ent ent_id like_req
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
boomLike st m_like ent ent_id like_req = do

  -- If m_like is Nothing, then we are creating a new "like".
  -- Otherwise, update an existing like
  e_like_req <- (case m_like of
       Nothing     -> rD $ postLike ent ent_id like_req
       (Just like) -> rD $ putLike' (like ^. _LikeResponse .. id_) like_req)

  case e_like_req of
       Left err   -> pure $ Left err
       Right resp -> do
         let like_id = resp ^. _LikeResponse .. id_
         e_stat <- rD $ getLikeStat' like_id
         case e_stat of
           Left err   -> pure $ Left err
           Right stat -> do
             -- Need to update stats AND our like
             pure $ Right $ st
