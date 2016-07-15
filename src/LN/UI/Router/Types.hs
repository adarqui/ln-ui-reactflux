{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}

-- |

module LN.UI.Router.Types (
  Routing,
  module CRUD,
  module Link,
  module OrderBy,
  module Routes,
  AppName,
  AppView,
  AppRouter,
  App (..),
  initApp
) where



import           Data.Maybe                 (Maybe)
import           Prelude                    (IO, pure, undefined)

import           LN.UI.Router.Class.CRUD    as CRUD
import           LN.UI.Router.Class.Link    as Link
import           LN.UI.Router.Class.OrderBy as OrderBy
import           LN.UI.Router.Class.Routes  as Routes


import           Data.Text                  (Text)
import           Data.Typeable              (Typeable)
import           React.Flux
-- import qualified Data.JSString.Text as JSS




type Routing e = ()





type AppName = Text
type AppView = ReactElementM ViewEventHandler
type AppRouter = [Text] -> IO ()

data App props = forall state. StoreData state =>
           App {appName        :: AppName
               , appState      :: ReactStore state
               , appView       :: Typeable props => state -> props -> AppView ()
               , appInitAction :: StoreAction state
               , appRouter     :: Maybe AppRouter
               }
               deriving Typeable

initApp :: Typeable props => App props -> IO (ReactView props)
initApp App{..} = do
--  let view' = defineControllerView (JSS.textToJSString appName) appState (\st props -> appView st props)
  let view' = defineControllerView "appName" appState (\st props -> appView st props)
  alterStore appState appInitAction
  pure view'
