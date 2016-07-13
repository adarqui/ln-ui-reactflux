module LN.UI.Router.Class.OrderBy (
  HasOrderBy,
  orderBy
) where



import           LN.T        (OrderBy)
import           LN.UI.Types (Array)



class HasOrderBy a where
  orderBy :: a -> Array OrderBy
