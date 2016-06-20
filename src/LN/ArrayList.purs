module LN.ArrayList (
  arrayToList,
  listToArray
) where



import Prelude       (($))
import Data.Array    as A
import Data.Array    ((:))
import Data.List     as L
import Data.List     (List(..))
import Data.Foldable as F



-- | arrayToList
--
arrayToList :: forall a. Array a -> List a
arrayToList xs = L.reverse $ F.foldl (\acc x -> (Cons x acc)) Nil xs



-- | listToArray
--
listToArray :: forall a. List a -> Array a
listToArray = go []
  where
  go acc Nil         = A.reverse acc
  go acc (Cons x xs) = go (x : acc) xs
