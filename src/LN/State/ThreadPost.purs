module LN.State.ThreadPost (
  ThreadPostRequestState,
  defaultThreadPostRequestState
) where



import Data.Maybe (Maybe(..))



type ThreadPostRequestState = {
  currentPrivateTag :: Maybe String,
  currentTag        :: Maybe String
}



defaultThreadPostRequestState :: ThreadPostRequestState
defaultThreadPostRequestState = {
  currentPrivateTag: Nothing,
  currentTag:        Nothing
}
