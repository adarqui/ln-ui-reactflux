module LN.Eval.Leurons (
  eval_GetLeurons
) where



import Halogen                       (get, gets, modify)
import Daimyo.Data.ArrayList         (listToArray, arrayToList)
import Data.Array                    (nub, filter, head)
import Data.Either                   (Either(..))
import Data.Map                      as M
import Data.Maybe                    (Maybe(..), maybe)
import Data.Tuple                    (Tuple(..))
import Optic.Core                    ((^.), (..))
import Prelude                       (bind, pure, not, map, ($), (+), (*), (-), (/))

import LN.Api                        (rd, getLeuronsCount')
import LN.Component.Types            (EvalEff)
import LN.Input.Types                (Input(..))
import LN.T



eval_GetLeurons :: EvalEff
eval_GetLeurons eval (GetLeurons next) = do
  pure next
