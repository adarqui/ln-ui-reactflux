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



-- | If m_like_response is Just, use this as the Ent and EntId instead of the supplied ent / ent_id
--
view
  :: Ent
  -> Int64
  -> Maybe LikeResponse
  -> HTMLView_

view !ent' !ent_id' !m_like_response' =
  case m_like_response' of
    Nothing -> view_ ent' ent_id' m_like_response'
    Just LikeResponse{..} -> view_ Ent_Like likeResponseId m_like_response'


view_
  :: Ent
  -> Int64
  -> Maybe LikeResponse
  -> HTMLView_

view_ !ent' !ent_id' !m_like' =
  defineViewWithSKey ("like-" <> (textToJSString' $ tshow ent_id')) (ent', ent_id', m_like') go
  where
  go (ent, ent_id, m_like) = do
    cldiv_ B.row $ do
      div_ [className_ color_like]    $ button_like (dispatch $ DoLike ent ent_id (req Like))
      div_ [className_ color_neutral] $ button_neutral (dispatch $ DoLike ent ent_id (req Neutral))
      div_ [className_ color_dislike] $ button_dislike (dispatch $ DoLike ent ent_id (req Dislike))
    where
    req k =
      case m_like of
        Nothing               -> Just $ LikeRequest k Nothing 0
        Just LikeResponse{..} -> if likeResponseOpt == k then Nothing else Just (LikeRequest k Nothing 0)

    color_like = case m_like of
                   Nothing               -> "like-none"
                   Just LikeResponse{..} -> case likeResponseOpt of
                                              Like -> "like-like"
                                              _    -> "like-none"

    color_neutral = case m_like of
                      Nothing               -> "like-none"
                      Just LikeResponse{..} -> case likeResponseOpt of
                                                 Neutral -> "like-neutral"
                                                 _       -> "like-none"

    color_dislike = case m_like of
                      Nothing               -> "like-none"
                      Just LikeResponse{..} -> case likeResponseOpt of
                                                 Dislike -> "like-dislike"
                                                 _       -> "like-none"
