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
import LN.T



eval_GetResources :: EvalEff
eval_GetResources eval (GetResources next) = do

  pageInfo <- gets _.resourcesPageInfo

  ecount <- rd $ getResourcesCount'
  case ecount of
    Left err -> pure next
    Right (CountResponses counts) -> do

      let count = maybe 0 (\(CountResponse count) -> count.n) (head counts.countResponses)

      modify (_ { resourcesPageInfo = pageInfo { totalResults = count, totalPages = (count / pageInfo.resultsPerPage)+1 } })

      eresource_packs <- rd $ getResourcePacks [Limit pageInfo.resultsPerPage, Offset ((pageInfo.currentPage-1) * pageInfo.resultsPerPage), SortOrder pageInfo.sortOrder, Order pageInfo.order]
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
