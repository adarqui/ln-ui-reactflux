module LN.Eval.Nop (
  eval_Nop
) where



import Control.Monad.Aff.Console (log)
import Halogen                   (liftAff')
import Prelude                   (bind, pure, ($))

import LN.Component.Types        (EvalEff)
import LN.Input.Types            (Input(..))



eval_Nop :: EvalEff
eval_Nop eval (Nop next) = do

  liftAff' $ log "nop"
  pure next
