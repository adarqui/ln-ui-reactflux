module LN.UI.Router.Crumb (
  HasCrumb,
  crumb
) where



class HasCrumb a where
  crumb :: a -> [a]
