{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module LN.UI.ReactFlux.App.Errors (
  view
) where



import           Control.Monad
import           Data.Text                             (Text)
import           React.Flux                            hiding (view)
import qualified React.Flux                            as RF

import           LN.UI.ReactFlux.Helpers.ReactFluxView
import           LN.UI.ReactFlux.Types



view :: [Text] -> HTMLView_
view !errors' =
  defineViewWithSKey "errors" errors' go
  where
  go :: [Text] -> HTMLView_
  go errors = do
    ul_ $ do
      forM_ (zip [1..] errors) $ \(idx, err) -> do
        li_ $ p_ $ elemText err
