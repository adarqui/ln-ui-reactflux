{-# LANGUAGE ExplicitForAll #-}

module LN.UI.ReactFlux.Sort (
  sortThreadPostPacks,
  sortThreadPacks,
  sortMapBy
) where



import qualified Data.List            as List
import           Data.Map             (Map)
import qualified Data.Map             as Map

import           LN.T.Pack.Thread
import           LN.T.Pack.ThreadPost
import           LN.T.Param
import           LN.T.Thread
import           LN.T.ThreadPost



-- sortThreadPostPacks :: SortOrder ->
sortThreadPostPacks SortOrderBy_Dsc = List.reverse . sortMapBy (_sortThreadPostPack threadPostResponseCreatedAt)
sortThreadPostPacks _               = sortMapBy (_sortThreadPostPack threadPostResponseCreatedAt)



_sortThreadPostPack by pack = by $ threadPostPackResponseThreadPost pack



sortThreadPacks SortOrderBy_Dsc = List.reverse . sortMapBy (_sortThreadPack threadResponseActivityAt)
sortThreadPacks _               = sortMapBy (_sortThreadPack threadResponseActivityAt)



_sortThreadPack by pack = by $ threadPackResponseThread pack



sortMapBy :: forall a b k. Ord b => (a -> b) -> Map k a -> [a]
sortMapBy cmp posts_map =
  List.sortBy (\t1 t2 -> compare (cmp t1) (cmp t2)) $ Map.elems posts_map
