module LN.Eval.Resources (
  eval_GetResources,
  eval_GetResourceId,
  eval_GetResourceSid,
  eval_GetResourcesLeurons,
  eval_GetResourcesSiftLeurons
) where



import Halogen                       (gets, modify)
import Data.Array                    (zip)
import Data.Either                   (Either(..))
import Data.Int                      (fromString)
import Data.Map                      as M
import Data.Maybe                    (Maybe(..))
import Prelude                       (bind, pure, map, ($))

import LN.Api                        (rd, getResourcesCount', getResourcePacks, getResourcePack')
import LN.Component.Types            (EvalEff)
import LN.Input.Types                (Input(..))
import LN.State.PageInfo             (runPageInfo)
import LN.T                          (ResourcePackResponses(..), ResourcePackResponse(..))



eval_GetResources :: EvalEff
eval_GetResources eval (GetResources next) = do

  page_info <- gets _.resourcesPageInfo

  ecount <- rd $ getResourcesCount'
  case ecount of
    Left err -> pure next
    Right counts -> do

      let new_page_info = runPageInfo counts page_info

      modify (_ { resourcesPageInfo = new_page_info.pageInfo })

      eresource_packs <- rd $ getResourcePacks new_page_info.params
      case eresource_packs of
           Left err -> pure next
           Right (ResourcePackResponses resource_packs) -> do

             let
              users = map (\(ResourcePackResponse pack) -> pack.user) resource_packs.resourcePackResponses
              resources_map =
                M.fromFoldable
                  $ zip (map (\(ResourcePackResponse p) -> p.resourceId) resource_packs.resourcePackResponses) resource_packs.resourcePackResponses


             eval (GetUsers_MergeMap_ByUser users next)


             modify (_{ resources = resources_map })
             pure next



eval_GetResourceId :: EvalEff
eval_GetResourceId eval (GetResourceId resource_id next) = do

  epack <- rd $ getResourcePack' resource_id
  case epack of
    Left err   -> pure next
    Right pack -> do
      modify (_{ currentResource = Just pack })
      pure next



eval_GetResourceSid :: EvalEff
eval_GetResourceSid eval (GetResourceSid resource_sid next) = do

  case fromString resource_sid of
       Nothing          -> pure next
       Just resource_id -> eval (GetResourceId resource_id next)



eval_GetResourcesLeurons :: EvalEff
eval_GetResourcesLeurons eval (GetResourcesLeurons resource_sid next) = do

  pure next



eval_GetResourcesSiftLeurons :: EvalEff
eval_GetResourcesSiftLeurons eval (GetResourcesSiftLeurons resource_sid next) = do

  pure next
