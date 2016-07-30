{-# LANGUAGE OverloadedStrings #-}

module LN.UI.ReactFlux.Main (
  runMain,
  runReactMain
) where



import           React.Flux

import qualified LN.UI.ReactFlux.App.Core   as Core
import           LN.UI.ReactFlux.Dispatcher



runMain :: IO ()
runMain = pure ()



runReactMain :: IO ()
runReactMain = do

  dispatcher

  reactRender
    "ln"
    Core.view
    ()

  dispatch $ SomeStoreAction Core.store Core.Init

  Core.initRouter
