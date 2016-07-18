{-# LANGUAGE OverloadedStrings #-}

module LN.UI.Main (
  runMain,
  runReactMain
) where



import qualified LN.UI.Router.Core       as Core
import qualified LN.UI.Router.Core.Store as Core
import           React.Flux



runMain :: IO ()
runMain = pure ()



runReactMain :: IO ()
runReactMain = do

  reactRender
    "ln"
    Core.coreView
    Core.defaultCoreStore

  Core.initCoreRouter
