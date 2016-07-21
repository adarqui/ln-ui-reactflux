{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module LN.UI.Router.Core where



import           Control.Monad.IO.Class      (liftIO)
import           GHC.Generics                ()
import           LN.UI.Router.Class.Routes
import           LN.UI.Router.Core.Store
import           React.Flux
import           React.Flux.Router.WebRoutes (initRouterRaw'ByteString)



initCoreRouter :: IO ()
initCoreRouter =
  initRouterRaw'ByteString (Just go) go
  where
  go = \raw_uri -> do
    print raw_uri
    routeAlterStore $ toRoutesWithHash raw_uri
    where
      routeAlterStore action =
        -- Update CoreStore with our new route
        liftIO $ alterStore coreStore $ Core_Route action
