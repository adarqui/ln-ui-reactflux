{-# LANGUAGE OverloadedStrings #-}

module LN.UI.ReactFlux.Main (
  runMain,
  runReactMain
) where



import qualified LN.UI.ReactFlux.App.Core as Core
import           React.Flux



runMain :: IO ()
runMain = pure ()



runReactMain :: IO ()
runReactMain = do

  reactRender
    "ln"
    Core.view
    ()

  executeAction $ SomeStoreAction Core.store Core.Init

  Core.initRouter
