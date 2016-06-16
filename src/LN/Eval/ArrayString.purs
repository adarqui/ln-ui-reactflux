module LN.Eval.ArrayString (
  eval_ArrayString
) where



import Data.Array                      (head, deleteAt, updateAt, nub, sort, (:))
import Data.Either                     (Either(..))
import Data.Functor                    (($>))
import Data.Map                        as M
import Data.Maybe                      (Maybe(..), maybe)
import Halogen                         (gets, modify)
import Optic.Core                      ((^.), (..), (.~))
import Prelude                         (class Eq, id, bind, pure, const, ($), (<>), (<<<))

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
    SetCurrent ent tag        -> mod (\st -> st{ currents = M.update (const $ Just tag) ent st.currents })
    AddFromCurrent ent        -> add_from_current ent id
    AddFromCurrentSort ent    -> add_from_current ent sort
    AddFromCurrentNub ent     -> add_from_current ent nub
    AddFromCurrentSortNub ent -> add_from_current ent (nub <<< sort)
    Edit ent idx new          -> mod (\st -> st{ ents = M.alter (\m_arr -> Just $ maybe [] (\arr -> maybe arr id $ updateAt idx new arr) m_arr) ent st.ents })
    Delete ent idx            -> mod (\st -> st{ ents = M.alter (\m_arr -> Just $ maybe [] (\arr -> maybe arr id $ deleteAt idx arr) m_arr) ent st.ents })
    Clear ent                 -> mod (\st -> st{ currents = M.update (const Nothing) ent st.currents, ents = M.update (const Nothing) ent st.ents })
    Empty                     -> mod (\st -> st{ currents = M.empty, ents = M.empty })
    _   -> pure next

  where
  append :: forall a. Eq a => Maybe (Array a) -> a -> Maybe (Array a)
  append Nothing a    = Just [a]
  append (Just arr) a = Just $ nub $ arr <> [a]
  set v req           = Just (v req)
  mod   f             = modify (\st->st{ arrayStringSt = f st.arrayStringSt }) $> next

  add_from_current ent fix = mod (\st ->
      let
        _currents = M.delete ent st.currents
        _ents = case M.lookup ent st.currents, M.lookup ent st.ents of
          Just v, Just arr -> M.alter (\m_arr -> Just $ maybe [] (\arr -> fix $ v : arr) m_arr) ent st.ents
          _, _             -> st.ents
      in
        st{ currents = _currents, ents = _ents })
