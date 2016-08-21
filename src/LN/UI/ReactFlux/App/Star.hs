{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LN.UI.ReactFlux.App.Star (
  view
) where



import           Data.Int                              (Int64)
import           Data.Monoid                           ((<>))
import           React.Flux                            hiding (view)
import qualified React.Flux                            as RF
import qualified Web.Bootstrap3                        as B

import           LN.T
import           LN.UI.Core.Helpers.DataText           (tshow)
import           LN.UI.Core.Helpers.GHCJS              (textToJSString')
import           LN.UI.ReactFlux.App.Core.Shared
import           LN.UI.ReactFlux.Helpers.ReactFluxDOM
import           LN.UI.ReactFlux.Helpers.ReactFluxView
import           LN.UI.ReactFlux.Types                 (HTMLView_)
import           LN.UI.ReactFlux.View.Button



-- | If m_star_response is Just, use this as the Ent and EndId instead of the supplied ent / ent_id
--
view
  :: Ent
  -> Int64
  -> Maybe StarResponse
  -> HTMLView_

view !ent' !ent_id' !m_star_response' =
  case m_star_response' of
    Nothing               -> view_ ent' ent_id' m_star_response'
    Just StarResponse{..} -> view_ Ent_Star starResponseId m_star_response'


view_
  :: Ent
  -> Int64
  -> Maybe StarResponse
  -> HTMLView_

view_ !ent' !ent_id' !m_star_response' = do
  defineViewWithSKey ("star-" <> (textToJSString' $ tshow ent_id')) (ent', ent_id', m_star_response') go
  where
  go (ent, ent_id, m_star_response) = do
    cldiv_ B.row $ do
      case m_star_response of
        Nothing -> div_ $ button_starEmpty' ["star-none"] (dispatch $ DoStar ent ent_id (Just $ StarRequest Nothing 0))
        Just _  -> div_ $ button_star' ["star"] (dispatch $ DoStar ent ent_id Nothing)
