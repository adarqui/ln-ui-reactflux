module LN.Eval.Resources (
  eval_GetResources
) where



import Halogen                       (get, gets, modify)
import Daimyo.Data.ArrayList         (listToArray, arrayToList)
import Data.Array                    (nub, filter, head, zip)
import Data.Either                   (Either(..))
import Data.Map                      as M
import Data.Maybe                    (Maybe(..), maybe)
import Data.Tuple                    (Tuple(..))
import Optic.Core                    ((^.), (..))
import Prelude                       (bind, pure, not, map, ($), (+), (*), (-), (/))

import LN.Api                        (rd, getResourcesCount', getResourcePacks', getResourcePacks)
import LN.Api.Internal.String        as ApiS
import LN.Component.Types            (EvalEff)
import LN.Input.Types                (Input(..))
import LN.State.PageInfo
import LN.T



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
                  $ zip (map (\(ResourcePackResponse p) -> p.resource ^. _ResourceResponse .. id_) resource_packs.resourcePackResponses) resource_packs.resourcePackResponses


             eval (GetUsers_MergeMap_ByUser users next)


             modify (_{ resources = resources_map })
             pure next
