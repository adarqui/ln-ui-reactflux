module LN.Eval.Me (
  eval_GetMe
) where



import Data.Either                     (Either(..))
import Data.Maybe                      (Maybe(..))
import Halogen                         (modify)
import Optic.Core                      ((^.), (..))
import Prelude                         (bind, pure, ($))

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
      modify (_{ me = Just me, meId = (me ^. _UserPackResponse .. user_ ^. _UserResponse .. id_) })
      pure next
