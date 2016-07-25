module LN.UI.Helpers.DataList (
  deleteNth
) where



import           Data.List   (splitAt, tail)
import           Data.Monoid ((<>))



deleteNth :: Int -> [a] -> [a]
deleteNth nth as = xs <> tail ys
  where
  (xs, ys) = splitAt nth as
