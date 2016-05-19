module LN.Eval.Leurons (
  eval_GetLeurons
) where



import Halogen                       (gets, modify)
import Data.Array                    (zip)
import Data.Either                   (Either(..))
import Data.Int                      (fromString)
import Data.Map                      as M
import Data.Maybe                    (Maybe(..))
import Prelude                       (bind, pure, map, ($))

import LN.Api                        (rd, getLeuronsCount', getLeuronPack', getLeuronPacks)
import LN.Component.Types            (EvalEff)
import LN.Input.Types                (Input(..))
import LN.State.PageInfo             (runPageInfo)
import LN.T                          (LeuronPackResponses(..), LeuronPackResponse(..))



eval_GetLeurons :: EvalEff
eval_GetLeurons eval (GetLeurons next) = do

  page_info <- gets _.leuronsPageInfo

  ecount <- rd $ getLeuronsCount'
  case ecount of
    Left err -> pure next
    Right counts -> do

      let new_page_info = runPageInfo counts page_info

      modify (_ { leuronsPageInfo = new_page_info.pageInfo })

      eleuron_packs <- rd $ getLeuronPacks new_page_info.params
      case eleuron_packs of
           Left err -> pure next
           Right (LeuronPackResponses leuron_packs) -> do

             let
              users = map (\(LeuronPackResponse pack) -> pack.user) leuron_packs.leuronPackResponses
              leurons_map =
                M.fromFoldable
                  $ zip (map (\(LeuronPackResponse p) -> p.leuronId) leuron_packs.leuronPackResponses) leuron_packs.leuronPackResponses


             eval (GetUsers_MergeMap_ByUser users next)


             modify (_{ leurons = leurons_map })
             pure next



eval_GetLeuronId :: EvalEff
eval_GetLeuronId eval (GetLeuronId leuron_id next) = do

  epack <- rd $ getLeuronPack' leuron_id
  case epack of
    Left err   -> pure next
    Right pack -> do
      modify (_{ currentLeuron = Just pack })
      pure next



eval_GetLeuronSid :: EvalEff
eval_GetLeuronSid eval (GetLeuronSid leuron_sid next) = do

  case fromString leuron_sid of
       Nothing          -> pure next
       Just leuron_id -> eval (GetLeuronId leuron_id next)
