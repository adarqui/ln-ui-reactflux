{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module LN.UI.Router.Core where



import           Control.Applicative         ((<|>))
import           Control.Monad.IO.Class      (liftIO)
import           Data.Typeable               (Typeable)
import           GHC.Generics                ()
import           LN.UI.Router.Class.Routes2
import           LN.UI.Router.Core.Store
import           React.Flux
import           React.Flux.Router.WebRoutes (initRouter)
import           Web.Routes



initCoreRouter :: IO ()
initCoreRouter =
  initRouter (Just go) go
  where
  go = \segments -> do
    either (const routeError) id $ runSite "" site segments
    where
      site = mkSitePI $ runRouteT routeAlterStore
      routeAlterStore action =
        -- Update CoreStore with our new route
        liftIO $ alterStore coreStore $ Core_Route action
      routeError = alterStore coreStore $ Core_Route Home
