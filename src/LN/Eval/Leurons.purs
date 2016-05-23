module LN.Eval.Leurons (
  eval_GetLeurons,
  eval_GetLeuronId,
  eval_GetLeuronSid
) where



import Halogen                       (gets, modify)
import Data.Either                   (Either(..))
import Data.Int                      (fromString)
import Data.Maybe                    (Maybe(..))
import Prelude                       (bind, pure, map, ($))

import LN.Api                        (rd, getLeuronsCount', getLeuronPack', getLeuronPacks)
import LN.Component.Types            (EvalEff)
import LN.Helpers.Map                (idmapFrom)
import LN.Input.Types                (Input(..))
import LN.State.PageInfo             (runPageInfo)
import LN.T                          ( LeuronPackResponses(..), LeuronPackResponse(..)
                                     , Param(..), SortOrderBy(..))



eval_GetLeurons :: EvalEff
eval_GetLeurons eval (GetLeurons next) = do

  page_info <- gets _.leuronsPageInfo

  e_count <- rd $ getLeuronsCount'
  case e_count of
    Left err     -> eval (AddErrorApi "eval_GetLeurons::getLeuronsCount'" err next)
    Right counts -> do

      let new_page_info = runPageInfo counts page_info

      modify (_ { leuronsPageInfo = new_page_info.pageInfo })

      e_leuron_packs <- rd $ getLeuronPacks new_page_info.params
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

  e_pack <- rd $ getLeuronPack' leuron_id
  case e_pack of
    Left err   -> pure next
    Right pack -> do
      modify (_{ currentLeuron = Just pack })
      pure next



eval_GetLeuronSid :: EvalEff
eval_GetLeuronSid eval (GetLeuronSid leuron_sid next) = do

  case fromString leuron_sid of
       Nothing          -> pure next
       Just leuron_id   -> eval (GetLeuronId leuron_id next)



eval_GetLeuronRandom :: EvalEff
eval_GetLeuronRandom eval (GetLeuronRandom next) = do

  e_packs <- rd $ getLeuronPacks [Limit 1, SortOrder SortOrderBy_Rnd]
  case e_packs of
    Left err                         -> eval (AddErrorApi "eval_GetLeuronRandom::getLeuronPacks" err next)
    Right (LeuronPackResponse packs) -> do
      case head packs.leuronPackResponses of
        Nothing   -> eval (AddError "eval_GetLeuronRandom" "Empty leuron response" next)
        Just pack -> modify (_{ currentLeuron = Just pack }) $> next
