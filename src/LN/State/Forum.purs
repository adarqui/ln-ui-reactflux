module LN.State.Forum (
  ForumRequestState,
  defaultForumRequestState
) where



import Data.Maybe (Maybe(..))



type ForumRequestState = {
  currentTag :: Maybe String
}



defaultForumRequestState :: ForumRequestState
defaultForumRequestState = {
  currentTag: Nothing
}
