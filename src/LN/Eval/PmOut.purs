module LN.Eval.PmOut (
  eval_PmOut
) where



import Data.Array                      (head, deleteAt, modifyAt, nub, sort, catMaybes, (:))
import Data.Ebyam                      (ebyam)
import Data.Either                     (Either(..))
import Data.Functor                    (($>))
import Data.Map                        as M
import Data.Maybe                      (Maybe(..), maybe)
import Data.String                     (toLower)
import Halogen                         (gets, modify)
import Optic.Core                      ((^.), (..), (.~))
import Prelude                         (class Eq, id, bind, pure, map, ($), (<>), (<<<))

import LN.Api                          (rd)
import LN.Api.Internal.String          as ApiS
import LN.Component.Types              (EvalEff)
import LN.Helpers.Map                  (idmapFrom)
import LN.Input.PmOut                     (InputPmOut(..), PmOut_Act(..), PmOut_Mod(..))
import LN.Input.Types                  (Input(..))
import LN.State.Loading                ()
import LN.State.Loading.Helpers        (setLoading, clearLoading)
import LN.Router.Class.Routes          (Routes(..))
import LN.Router.Class.CRUD            (CRUD(..))
import LN.Router.Class.Params          (emptyParams)
import LN.State.PageInfo               (runPageInfo)
import LN.T.Internal.Convert           ()
import LN.T



eval_PmOut :: EvalEff
eval_PmOut eval (CompPmOut sub next) = do

  case sub of

    InputPmOut_Act q -> do
      case q of
        _ -> pure next



    InputPmOut_Mod q -> do
      case q of
        _ -> pure next


    _   -> pure next

 where
  append :: forall a. Eq a => Maybe (Array a) -> a -> Maybe (Array a)
  append Nothing a    = Just [a]
  append (Just arr) a = Just $ nub $ arr <> [a]
  set v req           = Just (v req)
  mod new             = modify (\st->st{ currentPmOutRequest = maybe Nothing new st.currentPmOutRequest }) $> next
