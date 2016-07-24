module LN.UI.Router.LinkName (
  HasLinkName,
  linkName
) where



import           Data.Text (Text)



class HasLinkName a where
  linkName :: a -> Text
