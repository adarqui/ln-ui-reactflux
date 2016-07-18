{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module LN.UI.Router.Core where



import           Control.Applicative                      ((<|>))
import           Control.Monad.IO.Class                   (liftIO)
import           Data.Typeable                            (Typeable)
import           GHC.Generics                             ()
import           React.Flux
import           React.Flux.Router.WebRoutes              (initRouter)
import           Web.Routes
import           LN.UI.Router.Core.Store
import LN.UI.Router.Class.App



initCoreRouter :: IO ()
initCoreRouter =
  initRouter (Just go) go
  where
  go = \segments -> do
    either (const routeError) id $ runSite "" site segments
    where
      site = mkSitePI $ runRouteT routerAlterStore
      routerAlterStore action = do
        case action of
          AppHome  -> liftIO $ alterStore coreStore Core_Nop
          AppAbout -> liftIO $ alterStore coreStore Core_Nop
          _        -> liftIO $ alterStore coreStore Core_Nop

      routeError = alterStore coreStore $ Core_SetHash "#/"
