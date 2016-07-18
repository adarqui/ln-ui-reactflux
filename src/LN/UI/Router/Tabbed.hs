{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeFamilies       #-}

module LN.UI.Router.Tabbed (
  TabbedAction(..),
  TabbedState(..),
  defaultTabbedState,
  Tab(..),
  ParentRouter,
  dispatch,
  newStore,
  view,
  view_
) where



import Control.Monad.IO.Class (liftIO)
import           Control.Applicative   ((<|>))
import           Control.DeepSeq
import Data.Text (Text)
import qualified Data.Aeson            as A
import           Data.Maybe            (fromMaybe)
import qualified Data.Text             as T
import qualified Data.Text.Read        as TR
import           Data.Typeable         (Typeable)
import           GHC.Generics          (Generic)
import qualified Web.Routes            as WR

import           React.Flux            hiding (view)
import qualified React.Flux            as RF

import           LN.UI.Router.Internal
import           LN.UI.Router.Types



type ParentRouter = Maybe ([T.Text] -> T.Text)



data Tab = Tab {
  tabName   :: AppName,
  tabView   :: ParentRouter -> AppView ()
--  tabRouter :: Maybe AppRouter
}



data TabbedState = TabbedState {
  tsState       :: !Int,
  tsCurrentPage :: !TabbedAction,
  tsAboutApp    :: App (),
  tsHomeApp     :: App (),
  tsView        :: AppView ()
} deriving (Show, Typeable)

defaultTabbedState :: TabbedState
defaultTabbedState = TabbedState {
  tsState       = 0,
  tsCurrentPage = R_Home,
  tsAboutApp    = undefined,
  tsHomeApp     = undefined,
  tsView        = undefined
}



data TabbedAction
  = R_About
  | R_Home
  | R_404
  deriving (Show, Enum, Typeable, Generic, NFData)



instance WR.PathInfo TabbedAction where
  toPathSegments route =
    case route of
--      SwitchApp aidx apath -> "switchapp":WR.toPathSegments aidx ++ fromMaybe [] apath
      R_About              -> ["about"]
      R_Home               -> ["home"]
  fromPathSegments =
        -- SwitchApp
        --   <$ WR.segment "switchapp"
        --   <*> WR.pToken ("app-num"::String) intParser
        --   <*> WR.patternParse subRouteParser
        R_About <$ WR.segment "about"
    <|> R_Home <$ WR.segment "home"
    <|> pure R_404
    -- where
    --   intParser v =
    --     case TR.decimal v of
    --     Right (aidx, "") -> Just aidx
    --     _ -> Nothing
    --   subRouteParser apath =
    --     Right $ if null apath then Nothing else Just apath



-- instance Show TabbedState where
--  showsPrec prec TabbedState{..} = showsPrec prec ("TabbedState" :: String, tsFocus, map tabName tsTabs)



instance StoreData TabbedState where
  type StoreAction TabbedState = TabbedAction
  transform action st@TabbedState{..} = do
    putStrLn $ "Action: " ++ show action
    putStrLn $ "State: " ++ show st
    case action of
      R_About  -> pure (st { tsCurrentPage = R_About })
--      R_About -> do
--        case tsAboutApp of
--          App { appView = av, appState = ast } -> pure (st { tsCurrentPage = R_About, tsView = av mempty ()})
      R_Home  -> pure (st { tsCurrentPage = R_Home })
      -- TabbedInit ->
      --   return st
      -- SwitchApp idx tabRoute
      --   | idx >= 0 && idx <= length tsTabs -> do
      --       let Tab{..} = tsTabs !! idx
      --       case (tabRouter, tabRoute) of
      --         (Just tr, Just rt) ->
      --           tr rt
      --         _ ->
      --           return ()
      --       return $ st{tsFocus = idx}
      --   | otherwise ->
      --     error $ "Application index is out of range " ++ show idx



-- newStore :: [Tab] -> ReactStore TabbedState
-- newStore tabs = mkStore $ TabbedState 0 tabs
newStore :: TabbedState -> ReactStore TabbedState
newStore initial_state = mkStore initial_state



view :: ReactStore TabbedState -> ParentRouter -> ReactView TabbedState
view _ prouter = defineView "tabbed" $ \TabbedState{..} ->

  div_ $ do

    div_ $ do
      homeNav_ prouter
      aboutNav_ prouter

    div_ $ do
      p_ $ elemText "body"


--    mapM_ (tabItem_ . ((prouter,tsFocus),)) $ zip [0..] $ map tabName tsTabs

  --   div_ ["className" $= "tabbed-internal-app"] $
  --     if tsFocus < length tsTabs
  --      then tabView (tsTabs !! tsFocus) (Just $ router tsFocus)
  --     else mempty
  -- where
  --   router cur action =
  --     actionRoute prouter $ SwitchApp cur (Just $ childRoutePath action)
--   div_ $ p_ $ elemText "tabbed view"

  -- div_ $ do
  --   div_ ["className" $= "tabbed-app-picker"] $
  --     mapM_ (tabItem_ . ((prouter,tsFocus),)) $ zip [0..] $ map tabName tsTabs
  --   div_ ["className" $= "tabbed-internal-app"] $
  --     if tsFocus < length tsTabs
  --      then tabView (tsTabs !! tsFocus) (Just $ router tsFocus)
  --     else mempty
  -- where
  --   router cur action =
  --     actionRoute prouter $ SwitchApp cur (Just $ childRoutePath action)



-- tabItem :: ReactView ((ParentRouter, Int), (Int, AppName))
-- tabItem =
--   defineView "tab-item" $ \((prouter, cur), (aidx, aname)) ->
--   span_ ["style" @= A.object ["color" A..= ("#eee"::String)] | cur == aidx] $
--   if cur == aidx
--   then elemText aname
--   else a_ ["href" &= actionRoute prouter (SwitchApp aidx Nothing)] $ elemText aname



aboutNav :: ReactView ParentRouter
aboutNav =
  defineView "about-nav" $ \prouter ->
    span_ ["style" @= A.object ["color" A..= ("#eee"::String)]] $ a_ ["href" &= actionRoute prouter R_About] $ elemText "About"



aboutNav_ :: ParentRouter -> ReactElementM eventHandler ()
aboutNav_ tab =
  viewWithIKey aboutNav (fromEnum R_About) tab mempty



homeNav :: ReactView ParentRouter
homeNav =
  defineView "home-nav" $ \prouter ->
    span_ ["style" @= A.object ["color" A..= ("#eee"::String)]] $ a_ ["href" &= actionRoute prouter R_Home] $ elemText "Home"



homeNav_ :: ParentRouter -> ReactElementM eventHandler ()
homeNav_ tab =
  viewWithIKey homeNav (fromEnum R_Home) tab mempty



-- tabItem_ :: ((ParentRouter, Int), (Int, AppName)) -> ReactElementM eventHandler ()
-- tabItem_ tab =
--   viewWithKey tabItem (fst $ snd tab) tab mempty



view_ :: ReactStore TabbedState -> ParentRouter -> TabbedState -> ReactElementM eventHandler ()
view_ rst pr st = do
  RF.view (view rst pr) st mempty



dispatch :: ReactStore TabbedState -> TabbedAction -> [SomeStoreAction]
dispatch rst a = [SomeStoreAction rst a]
