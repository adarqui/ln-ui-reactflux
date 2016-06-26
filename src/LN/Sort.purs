module LN.Sort (
  sortThreadPostPacks
) where



import Data.Array   as A
import Data.Map     as M
import Optic.Core   ((^.), (..))
import Prelude      (compare, ($), (<<<))

import LN.ArrayList (listToArray)
import LN.T



sortThreadPostPacks SortOrderBy_Dsc = A.reverse <<< sortMapBy (_sortThreadPostPack createdAt_)
sortThreadPostPacks _               = sortMapBy (_sortThreadPostPack createdAt_)



sortMapBy cmp posts_map =
  A.sortBy (\t1 t2 -> compare (cmp t1) (cmp t2)) $ listToArray $ M.values posts_map



_sortThreadPostPack by pack = (pack ^. _ThreadPostPackResponse .. threadPost_ ^. _ThreadPostResponse .. by)
