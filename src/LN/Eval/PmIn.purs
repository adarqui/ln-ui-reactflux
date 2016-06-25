module LN.Eval.PmIn (
  eval_PmIn
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
import LN.Input.PmIn                     (InputPmIn(..), PmIn_Act(..), PmIn_Mod(..))
import LN.Input.Types                  (Input(..))
import LN.State.Loading                ()
import LN.State.Loading.Helpers        (setLoading, clearLoading)
import LN.Router.Class.Routes          (Routes(..))
import LN.Router.Class.CRUD            (CRUD(..))
import LN.Router.Class.Params          (emptyParams)
import LN.State.PageInfo               (runPageInfo)
import LN.T.Internal.Convert           ()
import LN.T



eval_PmIn :: EvalEff
eval_PmIn eval (CompPmIn sub next) = do

  case sub of

    InputPmIn_Act q -> do
      case q of
        _ -> pure next



    InputPmIn_Mod q -> do
      case q of
        _ -> pure next


    _   -> pure next

 where
  append :: forall a. Eq a => Maybe (Array a) -> a -> Maybe (Array a)
  append Nothing a    = Just [a]
  append (Just arr) a = Just $ nub $ arr <> [a]
  set v req           = Just (v req)
  mod new             = modify (\st->st{ currentPmInRequest = maybe Nothing new st.currentPmInRequest }) $> next
