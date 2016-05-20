module LN.Eval.Me (
  eval_GetMe
) where



import Control.Monad.Aff.Console       (log)
import Data.Either                     (Either(..))
import Data.Functor                    (($>))
import Data.Maybe                      (Maybe(..))
import Halogen                         (modify, liftAff')
import Optic.Core                      ((^.), (..))
import Prelude                         (bind, pure, show, ($), (<>))

import LN.Api                          (rd, getMePack')
import LN.Component.Types              (EvalEff)
import LN.Input.Types                  (Input(..))
import LN.T                            (_UserPackResponse, userId_)



eval_GetMe :: EvalEff
eval_GetMe eval (GetMe next) = do

  e_me <- rd $ getMePack'

  case e_me of

    Left err -> liftAff' (log $ "eval_GetMe:" <> show err) $> next

    Right me -> do
      modify (_{ me = Just me, meId = (me ^. _UserPackResponse .. userId_) })
      pure next
