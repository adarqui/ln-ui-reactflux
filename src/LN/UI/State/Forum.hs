module LN.UI.State.Forum (
  ForumRequestState (..),
  defaultForumRequestState
) where



import           Data.Maybe (Maybe (..))



data ForumRequestState = ForumRequestState {
  currentTag :: Maybe String
}



defaultForumRequestState :: ForumRequestState
defaultForumRequestState = ForumRequestState {
  currentTag = Nothing
}
