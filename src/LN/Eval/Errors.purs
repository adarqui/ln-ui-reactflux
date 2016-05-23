module LN.Eval.Errors (
  eval_AddError,
  eval_DelError,
  eval_ClearErrors
) where



import Control.Monad.Aff.Console       (log)
import Data.Array                      (deleteAt)
import Data.Either                     (Either(..))
import Data.Functor                    (($>))
import Data.Maybe                      (Maybe(..))
import Data.Tuple                      (Tuple(..))
import Halogen                         (gets, modify, liftAff')
import Optic.Core                      ((^.), (..))
import Prelude                         (bind, pure, show, ($), (<>))

import LN.Component.Types              (EvalEff)
import LN.Input.Types                  (Input(..))



eval_AddError :: EvalEff
eval_AddError eval (AddError author err next) = do

  modify (\st -> st{ errors = st.errors <> [Tuple author err] })
  pure next



eval_DelError :: EvalEff
eval_DelError eval (DelError index next) = do

  errors <- gets _.errors

  case deleteAt index errors of
       Nothing -> pure next
       Just arr -> modify (_{ errors = arr }) $> next



eval_ClearErrors :: EvalEff
eval_ClearErrors eval (ClearErrors next) = modify (_{ errors = [] }) $> next
