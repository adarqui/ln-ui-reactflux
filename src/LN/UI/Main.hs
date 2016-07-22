{-# LANGUAGE OverloadedStrings #-}

module LN.UI.Main (
  runMain,
  runReactMain
) where



import qualified LN.UI.App.Core as Core
import           React.Flux



runMain :: IO ()
runMain = pure ()



runReactMain :: IO ()
runReactMain = do

  reactRender
    "ln"
    Core.view
    Core.defaultStore

  executeAction $ SomeStoreAction Core.store Core.Init

  Core.initRouter
