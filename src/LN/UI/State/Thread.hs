module LN.UI.State.Thread (
  ThreadRequestState,
  defaultThreadRequestState
) where



import           Data.Maybe (Maybe (..))



data ThreadRequestState = ThreadRequestState {
  currentTag :: Maybe String
}



defaultThreadRequestState :: ThreadRequestState
defaultThreadRequestState = ThreadRequestState {
  currentTag = Nothing
}
