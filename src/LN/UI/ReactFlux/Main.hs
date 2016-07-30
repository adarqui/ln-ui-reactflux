{-# LANGUAGE OverloadedStrings #-}

module LN.UI.ReactFlux.Main (
  runMain,
  runReactMain
) where



import           React.Flux

import qualified LN.UI.ReactFlux.App.Core as Core
import LN.UI.ReactFlux.Dispatch



runMain :: IO ()
runMain = pure ()



runReactMain :: IO ()
runReactMain = do

  dispatcher

  reactRender
    "ln"
    Core.view
    ()

--  executeAction $ SomeStoreAction Core.store Core.Init
  dispatch $ SomeStoreAction Core.store Core.Init

  Core.initRouter
