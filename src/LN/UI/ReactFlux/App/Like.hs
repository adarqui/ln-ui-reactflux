{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LN.UI.ReactFlux.App.Like (
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
import           LN.UI.Core.State.Internal             (Action (..))
import           LN.UI.ReactFlux.App.Core.Shared
import           LN.UI.ReactFlux.Helpers.ReactFluxDOM
import           LN.UI.ReactFlux.Helpers.ReactFluxView
import           LN.UI.ReactFlux.Types                 (HTMLView_)
import           LN.UI.ReactFlux.View.Button



view
  :: Ent
  -> Int64
  -> Maybe LikeResponse
  -> HTMLView_

view !ent' !ent_id' !m_like' =
  defineViewWithSKey ("like-" <> (textToJSString' $ tshow ent_id')) (ent', ent_id', m_like') go
  where
  go (ent, ent_id, m_like) = do
    cldiv_ B.row $ do
      p_ $ elemText "like"
      div_ [className_ color_like]    $ button_like (dispatch $ DoLike ent ent_id Nothing)
      div_ [className_ color_neutral] $ button_neutral (dispatch $ DoLike ent ent_id Nothing)
      div_ [className_ color_dislike] $ button_dislike (dispatch $ DoLike ent ent_id Nothing)
    where
    color_like = case m_like of
                   Nothing               -> "like-black"
                   Just LikeResponse{..} -> case likeResponseOpt of
                                              Like -> "like-green"
                                              _    -> "like-black"

    color_neutral = case m_like of
                      Nothing               -> "neutral-black"
                      Just LikeResponse{..} -> case likeResponseOpt of
                                                 Neutral -> "neutral-yellow"
                                                 _       -> "neutral-black"

    color_dislike = case m_like of
                      Nothing               -> "dislike-black"
                      Just LikeResponse{..} -> case likeResponseOpt of
                                                 Dislike -> "dislike-red"
                                                 _       -> "dislike-black"
