{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module LN.UI.ReactFlux.App.Like (
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
  -> Maybe LikeResponse
  -> HTMLView_

view !ent' !ent_id' !m_like' =
  defineViewWithSKey ("like-" <> (textToJSString' $ tshow ent_id')) (ent', ent_id', m_like') go
  where
  go (ent, ent_id, m_like) = do
    div_ $ p_ $ elemText "like"
