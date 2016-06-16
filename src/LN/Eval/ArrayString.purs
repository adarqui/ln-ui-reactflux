module LN.Eval.ArrayString (
  eval_ArrayString
) where



import Data.Array                      (head, deleteAt, modifyAt, nub, sort, (:))
import Data.Either                     (Either(..))
import Data.Functor                    (($>))
import Data.Map                        as M
import Data.Maybe                      (Maybe(..), maybe)
import Halogen                         (gets, modify)
import Optic.Core                      ((^.), (..), (.~))
import Prelude                         (class Eq, id, bind, pure, const, ($), (<>))

import LN.Api.Internal.String          as ApiS
import LN.Component.Types              (EvalEff)
import LN.Helpers.Map                  (idmapFrom)
import LN.Input.ArrayString            (InputArrayString(..))
import LN.Input.Types                  (Input(..))
import LN.State.Loading.Helpers        (setLoading, clearLoading)
import LN.State.ArrayString            (ArrayStringState, defaultArrayStringState)



eval_ArrayString :: EvalEff
eval_ArrayString eval (CompArrayString sub next) = do

  case sub of
    SetCurrent ent tag -> mod (\st -> st{ currents = M.update (const $ Just tag) ent st.currents })
--    Setag ent tag    -> modSt (\(ArrayStringState req)-> Just $ 
--        AddTag               -> do
--          m_current_tag <-
--          mod (\(ArrayStringRequest req)->Just $ ArrayStringRequest req{tags = nub $ sort (tag : req.tags)})
--        DeleteTag idx        -> mod (\(ArrayStringRequest req)->Just $ ArrayStringRequest req{tags = maybe req.tags id (deleteAt idx req.tags) })
--        ClearTags            -> mod $ set (\req -> _ArrayStringRequest .. tags_ .~ [] $ req)
    _   -> pure next

  where
  append :: forall a. Eq a => Maybe (Array a) -> a -> Maybe (Array a)
  append Nothing a    = Just [a]
  append (Just arr) a = Just $ nub $ arr <> [a]
  set v req           = Just (v req)
  mod   f             = modify (\st->st{ arrayStringSt = f st.arrayStringSt }) $> next
--  modSt new           = modify (\st->st{ currentArrayStringSt = maybe Nothing (Just <<< new) st.currentArrayStringSt })
