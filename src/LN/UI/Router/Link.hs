module LN.UI.Router.Link (
  HasLink (..),
) where



import           LN.UI.Router.Param (Params)
import           LN.UI.Types        (Tuple)



class HasLink a where
  link :: a -> Tuple String Params
