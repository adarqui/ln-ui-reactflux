module LN.Eval.Leurons (
  eval_GetLeurons,
  eval_GetLeuronId,
  eval_GetLeuronRandom,
  eval_Leuron
) where



import Data.Array                    (head, deleteAt, modifyAt, nub)
import Data.Either                   (Either(..))
import Data.Functor                  (($>))
import Data.Int                      (fromString)
import Data.Map                      as M
import Data.Maybe                    (Maybe(..), maybe)
import Halogen                       (gets, modify)
import Optic.Core                    ((^.), (..), (.~))
import Prelude                       (class Eq, id, const, bind, pure, map, ($), (<>))

import LN.Api                        (rd, getLeuronsCount', getLeuronPack', getLeuronPacks)
import LN.Component.Types            (EvalEff)
import LN.Helpers.Map                (idmapFrom)
import LN.Input.Leuron               (InputLeuron(..), Leuron_Mod(..))
import LN.Input.Types                (Input(..))
import LN.State.Loading              (setLoading, clearLoading, l_currentLeuron, l_leurons)
import LN.State.PageInfo             (runPageInfo)
import LN.T                          ( LeuronPackResponses(..), LeuronPackResponse(..)
                                     , _LeuronRequest
                                     , title_, section_
                                     , Param(..), SortOrderBy(..))



eval_GetLeurons :: EvalEff
eval_GetLeurons eval (GetLeurons next) = do

  modify (_{ leurons = (M.empty :: M.Map Int LeuronPackResponse) })

  page_info <- gets _.leuronsPageInfo

  e_count <- rd $ getLeuronsCount'
  case e_count of
    Left err     -> eval (AddErrorApi "eval_GetLeurons::getLeuronsCount'" err next)
    Right counts -> do

      let new_page_info = runPageInfo counts page_info

      modify (_{ leuronsPageInfo = new_page_info.pageInfo })
      modify (\st->st{loading = setLoading l_leurons st.loading})

      e_leuron_packs <- rd $ getLeuronPacks new_page_info.params

      modify (\st->st{loading = clearLoading l_leurons st.loading})

      case e_leuron_packs of
           Left err                                 -> eval (AddErrorApi "eval_GetLeurons::getLeuronPacks" err next)
           Right (LeuronPackResponses leuron_packs) -> do

             let
              users       = map (\(LeuronPackResponse pack) -> pack.user) leuron_packs.leuronPackResponses
              leurons_map = idmapFrom (\(LeuronPackResponse p) -> p.leuronId) leuron_packs.leuronPackResponses


             eval (GetUsers_MergeMap_ByUser users next)


             modify (_{ leurons = leurons_map })
             pure next



eval_GetLeuronId :: EvalEff
eval_GetLeuronId eval (GetLeuronId leuron_id next) = do

  modify (_{ currentLeuron = Nothing })
  modify (\st->st{loading = setLoading l_currentLeuron st.loading})

  e_pack <- rd $ getLeuronPack' leuron_id

  modify (\st->st{loading = clearLoading l_currentLeuron st.loading})

  case e_pack of
    Left err   -> eval (AddErrorApi "eval_GetLeuronId::getLeuronPack'" err next)
    Right pack -> do
      modify (_{ currentLeuron = Just pack })
      pure next



eval_GetLeuronRandom :: EvalEff
eval_GetLeuronRandom eval (GetLeuronRandom next) = do

  modify (_{ currentLeuron = Nothing })
  modify (\st->st{loading = setLoading l_currentLeuron st.loading})

  e_packs <- rd $ getLeuronPacks [Limit 1, SortOrder SortOrderBy_Rnd]

  modify (\st->st{loading = clearLoading l_currentLeuron st.loading})

  case e_packs of
    Left err                          -> eval (AddErrorApi "eval_GetLeuronRandom::getLeuronPacks" err next)
    Right (LeuronPackResponses packs) -> do
      case head packs.leuronPackResponses of
        Nothing   -> pure next
        Just pack -> modify (_{ currentLeuron = Just pack }) $> next




eval_Leuron :: EvalEff
eval_Leuron eval (CompLeuron sub next) = do
  case sub of
    InputLeuron_Mod q -> do
      case q of
        SetTitle title      -> mod $ set (\req -> _LeuronRequest .. title_ .~ Just title $ req)
        SetSection section  -> mod $ set (\req -> _LeuronRequest .. section_ .~ Just section $ req)

    InputLeuron_Nop         -> pure next

  where
  append :: forall a. Eq a => Maybe (Array a) -> a -> Maybe (Array a)
  append Nothing a    = Just [a]
  append (Just arr) a = Just $ nub $ arr <> [a]
  set v req                = Just (v req)
  mod new                  = modify (\st->st{ currentLeuronRequest = maybe Nothing new st.currentLeuronRequest }) $> next
