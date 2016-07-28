{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- | A Generic deletion view, with Ok and Cancel
--
module LN.UI.ReactFlux.App.Delete (
  view_,
  view
) where



import           React.Flux                 hiding (view)
import qualified React.Flux                 as RF

import           LN.UI.ReactFlux.Helpers.ReactFluxDOM
import           LN.UI.Core.Router
import           LN.UI.ReactFlux.Types                (HTMLEvent_)



view_ :: HTMLEvent_
view_ =
  RF.view view () mempty



view :: ReactView ()
view = defineView "delete" $ \_ ->
  div_ $ p_ $ do
    h1_ "Delete"
