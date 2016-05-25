module LN.Helpers.Array (
  seqArrayFrom
) where



import Data.Array (zip, range, length)
import Data.Tuple (Tuple)
import Prelude    (($))



-- | creates a "sequentially identified array", indexed starting at 0
--
seqArrayFrom :: forall a. Array a -> Array (Tuple Int a)
seqArrayFrom arr = zip (range 0 $ length arr) arr
