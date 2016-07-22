module LN.UI.Router.Class.Link (
  HasLink (..),
) where



import           LN.UI.Router.Class.Param (Params)
import           LN.UI.Types            (Tuple)



class HasLink a where
  link :: a -> Tuple String Params
