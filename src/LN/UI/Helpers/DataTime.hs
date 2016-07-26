{-# LANGUAGE OverloadedStrings #-}

module LN.UI.Helpers.DataTime (
  prettyUTCTime,
  prettyUTCTimeMaybe
) where



import           Data.Text (Text)
import qualified Data.Text as Text (pack)
import           Data.Time



prettyUTCTime :: UTCTime -> Text
prettyUTCTime utc_time =
  Text.pack $ formatTime defaultTimeLocale "%B %Y" utc_time



prettyUTCTimeMaybe :: Maybe UTCTime -> Text
prettyUTCTimeMaybe Nothing = "No time."
prettyUTCTimeMaybe (Just utc_time) = prettyUTCTime utc_time
