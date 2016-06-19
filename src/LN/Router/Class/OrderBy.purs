module LN.Router.Class.OrderBy (
  class HasOrderBy,
  orderBy
) where



import LN.T                        (OrderBy)



class HasOrderBy a where
  orderBy :: a -> Array OrderBy
