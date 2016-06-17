module LN.State.Thread (
  ThreadRequestState,
  defaultThreadRequestState
) where



import Data.Maybe (Maybe(..))



type ThreadRequestState = {
  currentTag :: Maybe String
}



defaultThreadRequestState :: ThreadRequestState
defaultThreadRequestState = {
  currentTag: Nothing
}
