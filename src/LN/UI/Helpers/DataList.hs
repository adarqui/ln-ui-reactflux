module LN.UI.Helpers.DataList (
  deleteNth,
  tailFriendly,
  toSeqList
) where



import           Data.List   (splitAt, tail)
import           Data.Monoid ((<>))



deleteNth :: Int -> [a] -> [a]
deleteNth nth as = xs <> tailFriendly ys
  where
  (xs, ys) = splitAt nth as



tailFriendly :: [a] -> [a]
tailFriendly [] = []
tailFriendly xs = tail xs



toSeqList :: [a] -> [(Int, a)]
toSeqList = zip [0..]
