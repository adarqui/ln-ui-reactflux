module LN.State.Board (
  BoardRequestState,
  defaultBoardRequestState
) where



import Data.Maybe (Maybe(..))



type BoardRequestState = {
  currentSuggestedTag :: Maybe String,
  currentTag          :: Maybe String
}



defaultBoardRequestState :: BoardRequestState
defaultBoardRequestState = {
  currentSuggestedTag: Nothing,
  currentTag:          Nothing
}
