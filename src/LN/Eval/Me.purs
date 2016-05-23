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
import LN.T                            (_UserPackResponse, userId_)



eval_GetMe :: EvalEff
eval_GetMe eval (GetMe next) = do

  e_me <- rd $ getMePack'

  case e_me of

    Left err -> eval (AddErrorApi "eval_GetMe::getMePack'" err next)

    Right me -> do
      modify (_{ me = Just me, meId = (me ^. _UserPackResponse .. userId_) })
      pure next
