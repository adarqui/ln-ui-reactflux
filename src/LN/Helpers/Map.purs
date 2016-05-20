module LN.Helpers.Map (
  mergeMap,
  mergeMapList,
  mergeMapArray,
  idmapFrom
) where



import Data.Array as A
import Data.List  as L
import Data.Map   as M
import Optic.Core ((^.))
import Prelude    (map, ($))



mergeMap st m =
  M.union st m



mergeMapList st m un_accessor =
  M.union st (M.fromList $ L.zip (map un_accessor m) m)



mergeMapArray st m un_accessor =
  M.union st (M.fromFoldable $ A.zip (map un_accessor m) m)



idmapFrom un pack = M.fromFoldable $ A.zip (map (\pack -> un pack) pack) pack
