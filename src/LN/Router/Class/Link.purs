module LN.Router.Class.Link (
  class HasLink,
  link
) where



import Data.Tuple                  (Tuple)

import LN.Router.Class.Params      (Params)



class HasLink a where
  link :: a -> Tuple String Params
