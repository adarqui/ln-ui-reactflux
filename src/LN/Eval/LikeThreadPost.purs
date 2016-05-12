module LN.Eval.LikeThreadPost (
  eval_LikeThreadPost
) where



import Data.Maybe                  (Maybe(..), maybe)
import Data.Either                 (Either(..))
import Data.Functor                (($>))
import Halogen                     (get, modify, liftAff')
import Optic.Core                  ((^.), (..))
import Prelude                     (bind, pure, show, ($), (<>))

import LN.Api                      (rd, postThreadPostLike_ByThreadPostId')
import LN.Component.Types          (EvalEff)
import LN.Input.LikeThreadPost     (InputLikeThreadPost(..))
import LN.Input.Types              (Input(..))
import LN.State.Lens
import LN.T



eval_LikeThreadPost :: EvalEff



eval_LikeThreadPost eval (CompLikeThreadPost (InputLikeThreadPost_Like thread_post_id) next) = do
  let tplr = mkThreadPostLikeRequest Like Nothing
  etplr <- rd $ postThreadPostLike_ByThreadPostId' thread_post_id tplr
  case etplr of
    Left err    -> pure next
    Right rtplr -> pure next
  pure next



eval_LikeThreadPost eval (CompLikeThreadPost (InputLikeThreadPost_Neutral thread_post_id) next) = do
  let tplr = mkThreadPostLikeRequest Neutral Nothing
  pure next



eval_LikeThreadPost eval (CompLikeThreadPost (InputLikeThreadPost_Dislike thread_post_id) next) = do
  let tplr = mkThreadPostLikeRequest DontLike Nothing
  pure next



eval_LikeThreadPost eval (CompLikeThreadPost (InputLikeThreadPost_Star thread_post_id) next) = do
  pure next
