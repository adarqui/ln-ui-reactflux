module LN.UI.Helpers.Map (
  idmapFrom
) where



import           Data.List (zip)
import           Data.Map  (Map)
import qualified Data.Map  as Map



idmapFrom :: (Ord index) => (a -> index) -> [a] -> Map index a
idmapFrom un packs = Map.fromList $ zip (map un packs) packs
