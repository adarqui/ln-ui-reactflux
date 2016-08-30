{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

module LN.UI.ReactFlux.App.Experiments (
  view
) where



import           Data.Text                             (Text)
import           React.Flux                            hiding (view)
import qualified Web.Bootstrap3                        as B

import           LN.Generate.Default
import           LN.T
import qualified LN.UI.Core.App.ThreadPost             as ThreadPost
import           LN.UI.ReactFlux.App.Core.Shared
import qualified LN.UI.ReactFlux.App.NotFound          as NotFound
import qualified LN.UI.ReactFlux.App.ThreadPosts       as ThreadPost
import           LN.UI.ReactFlux.Helpers.ReactFluxDOM
import           LN.UI.ReactFlux.Helpers.ReactFluxView
import           LN.UI.ReactFlux.Helpers.ReactFluxView
import           LN.UI.ReactFlux.Types



view :: Text -> Store -> HTMLView_
view experiment_sid store =
  case experiment_sid of
    "re-render-1" -> viewReRender1 store
    "re-render-2" -> viewReRender2 store
    "re-render-3" -> viewReRender3 store True True (p_ $ elemText "hi")
    _             -> NotFound.view



viewReRender1 :: Store -> HTMLView_
viewReRender1 store@Store{..} = do
  testIFrame
  cldiv_ B.well $ do
    textarea_ [ className_ B.formControl
              , "rows" $= "10"
              , "value" @= body
              , onChange $ \input -> dispatch $ ThreadPost.setBody tpr (PostDataRaw $ targetValue input)
              ] mempty
  where
  tpr@ThreadPostRequest{..} = maybe defaultThreadPostRequest id _m_threadPostRequest
  body                      = ThreadPost.postDataToBody threadPostRequestBody




viewReRender2 :: Store -> HTMLView_
viewReRender2 !store' =
  defineViewWithSKey "experiments-re-render-2" store' go
  where
  go :: Store -> HTMLView_
  go store@Store{..} = do
    testIFrame
    cldiv_ B.well $ do
      textarea_ [ className_ B.formControl
                , "rows" $= "10"
                , "value" @= body
                , onChange $ \input -> dispatch $ ThreadPost.setBody tpr (PostDataRaw $ targetValue input)
                ] mempty
    where
    tpr@ThreadPostRequest{..} = maybe defaultThreadPostRequest id _m_threadPostRequest
    body                      = ThreadPost.postDataToBody threadPostRequestBody



viewReRender3 :: Store -> Bool -> Bool -> HTMLView_ -> HTMLView_
viewReRender3 !store' !tf1' !tf2' !plumbing' = do
  defineViewWithSKey "experiments-re-render-3" (store', tf1', tf2', plumbing') go
  where
  go (store, tf1, tf2, plumbing) = do
    plumbing
    viewReRender2 store



testIFrame :: HTMLView_
testIFrame = do
  iframe_ [ "src" $= "https://www.youtube.com/embed/AVWRQ21Iorc", "height" @= (405 :: Int), "width" @= (720 :: Int) ] mempty
