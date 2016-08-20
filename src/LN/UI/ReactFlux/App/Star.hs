{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module LN.UI.ReactFlux.App.Star (
  view
) where



import           Data.Int                              (Int64)
import           Data.Monoid                           ((<>))
import           React.Flux                            hiding (view)
import qualified React.Flux                            as RF

import           LN.T
import           LN.UI.Core.Helpers.DataText           (tshow)
import           LN.UI.Core.Helpers.GHCJS              (textToJSString')
import           LN.UI.ReactFlux.Helpers.ReactFluxView
import           LN.UI.ReactFlux.Types                 (HTMLView_)



view
  :: Ent
  -> Int64
  -> Maybe StarResponse
  -> HTMLView_

view !ent' !ent_id' !m_star' =
  defineViewWithSKey ("star-" <> (textToJSString' $ tshow ent_id')) (ent', ent_id', m_star') go
  where
  go (ent, ent_id, m_star) = do
    div_ $ p_ $ elemText "star"
