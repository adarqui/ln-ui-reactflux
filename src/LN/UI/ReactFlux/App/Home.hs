{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module LN.UI.ReactFlux.App.Home (
  view
) where



import           Control.DeepSeq (NFData)
import           Data.Typeable   (Typeable)
import           GHC.Generics    (Generic)
import           React.Flux      hiding (view)
import qualified React.Flux      as RF

import           LN.UI.ReactFlux.Types
import           LN.UI.ReactFlux.Helpers.ReactFluxView



view :: HTMLView_
view =
  defineViewWithSKey "home" () go
  where
  go _ = div_ $ p_ $ elemText "Welcome to LN!"
