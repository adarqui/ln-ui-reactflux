module LN.Eval.Nop (
  eval_Nop
) where



import Control.Monad.Aff.Console (log)
import Prelude                   (bind, pure, ($))

import LN.Component.Types        (EvalEff)
import LN.Input.Types            (Input(..))

import Control.Monad.Aff.Free (fromAff)


eval_Nop :: EvalEff
eval_Nop eval (Nop next) = pure next
--  fromAff $ log "nop"
--  pure next
