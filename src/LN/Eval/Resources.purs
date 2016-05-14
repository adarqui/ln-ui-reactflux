module LN.Eval.Resources (
  eval_GetResources
) where



import Halogen                       (get, gets, modify)
import Daimyo.Data.ArrayList         (listToArray, arrayToList)
import Data.Array                    (nub, filter, head)
import Data.Either                   (Either(..))
import Data.Map                      as M
import Data.Maybe                    (Maybe(..), maybe)
import Data.Tuple                    (Tuple(..))
import Optic.Core                    ((^.), (..))
import Prelude                       (bind, pure, not, map, ($), (+), (*), (-), (/))

import LN.Api                        (rd, getResourcesCount', getResourcePacks')
import LN.Api.Internal.String        as ApiS
import LN.Component.Types            (EvalEff)
import LN.Input.Types                (Input(..))
import LN.T



eval_GetResources :: EvalEff
eval_GetResources eval (GetResources next) = do

  eresource_packs <- rd $ getResourcePacks'
  case eresource_packs of
       Left err -> pure next
       Right (ResourcePackResponses resource_packs) -> do
         modify (_{ resources = resource_packs.resourcePackResponses })
         pure next
