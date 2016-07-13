module LN.UI.State.Board (
  BoardRequestState (..),
  defaultBoardRequestState
) where



import           Data.Maybe (Maybe (..))



data BoardRequestState = BoardRequestState {
  currentSuggestedTag :: Maybe String,
  currentTag          :: Maybe String
}



defaultBoardRequestState :: BoardRequestState
defaultBoardRequestState = BoardRequestState {
  currentSuggestedTag = Nothing,
  currentTag =          Nothing
}
