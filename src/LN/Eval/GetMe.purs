module LN.Eval.GetMe (
  eval_GetMe
) where



import Control.Monad.Aff.Console       (log)
import Data.Array                      (length)
import Data.Either                     (Either(..))
import Data.Functor                    (($>))
import Data.Maybe                      (Maybe(..), maybe)
import Halogen                         (get, modify, liftAff')
import Optic.Core                      ((^.), (..))
import Prelude                         (bind, pure, show, ($), (<>))

import LN.Api                          (rd, getMePack')
import LN.Component.Types              (EvalEff)
import LN.Input.Types                  (Input(..))
import LN.T.Internal.Types



eval_GetMe :: EvalEff
eval_GetMe eval (GetMe next) = do

  eme <- rd $ getMePack'
  case eme of
    Left err -> pure next
    Right me -> do
      modify (_{ me = Just me })
      pure next
