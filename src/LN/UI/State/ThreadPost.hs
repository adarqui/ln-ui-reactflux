module LN.UI.State.ThreadPost (
  ThreadPostRequestState (..),
  defaultThreadPostRequestState
) where



import           Data.Maybe (Maybe (..))



data ThreadPostRequestState = ThreadPostRequestState {
  currentPrivateTag :: Maybe String,
  currentTag        :: Maybe String
}



defaultThreadPostRequestState :: ThreadPostRequestState
defaultThreadPostRequestState = ThreadPostRequestState {
  currentPrivateTag = Nothing,
  currentTag =        Nothing
}
