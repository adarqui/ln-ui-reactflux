module LN.Eval.LikeThreadPost (
  eval_LikeThreadPost
) where



import Control.Monad.Aff.Console   (log)
import Data.Maybe                  (Maybe(..), maybe)
import Data.Either                 (Either(..))
import Data.Functor                (($>))
import Halogen                     (get, modify, liftAff')
import Optic.Core                  ((^.), (..))
import Prelude                     (bind, pure, show, ($), (<>))

import LN.Component.Types          (EvalEff)
import LN.Input.LikeThreadPost     (InputLikeThreadPost(..))
import LN.Input.Types              (Input(..))
import LN.State.Lens
import LN.T



eval_LikeThreadPost :: EvalEff



eval_LikeThreadPost eval (CompLikeThreadPost (InputLikeThreadPost_Like thread_post_id) next) = do
  pure next



eval_LikeThreadPost eval (CompLikeThreadPost (InputLikeThreadPost_Neutral thread_post_id) next) = do
  pure next



eval_LikeThreadPost eval (CompLikeThreadPost (InputLikeThreadPost_Dislike thread_post_id) next) = do
  pure next



eval_LikeThreadPost eval (CompLikeThreadPost (InputLikeThreadPost_Star thread_post_id) next) = do
  pure next
