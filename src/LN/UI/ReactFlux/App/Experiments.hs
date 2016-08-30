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
import           LN.UI.ReactFlux.App.Loader            (Loader (..))
import qualified LN.UI.ReactFlux.App.Loader            as Loader
import qualified LN.UI.ReactFlux.App.NotFound          as NotFound
import qualified LN.UI.ReactFlux.App.Oops              as Oops
import qualified LN.UI.ReactFlux.App.ThreadPosts       as ThreadPost
import           LN.UI.ReactFlux.Helpers.ReactFluxDOM
import           LN.UI.ReactFlux.Helpers.ReactFluxView
import           LN.UI.ReactFlux.Helpers.ReactFluxView
import           LN.UI.ReactFlux.Types



view :: Text -> Store -> HTMLView_
view experiment_sid store =
  case _m_threadPostRequest store of
    Nothing      -> Oops.view
    Just tpr ->
      case experiment_sid of
        "re-render-1" -> viewReRender1 tpr store
        "re-render-2" -> viewReRender2 tpr store
        "re-render-3" -> viewReRender3 tpr store True True (p_ $ elemText "hi")
        "re-render-4" -> viewReRender4 tpr store 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 (p_ $ elemText "hi")
        "re-render-5" -> viewReRender5 tpr store (p_ $ elemText "hi")
        _             -> NotFound.view



viewReRender1 :: ThreadPostRequest -> Store -> HTMLView_
viewReRender1 tpr store@Store{..} = do
  testIFrame
  cldiv_ B.well $ do
    textarea_ [ className_ B.formControl
              , "rows" $= "10"
              , "value" @= body
              , onChange $ \input -> dispatch $ ThreadPost.setBody tpr (PostDataRaw $ targetValue input)
              ] mempty
  where
  ThreadPostRequest{..} = tpr
  body                  = ThreadPost.postDataToBody threadPostRequestBody




viewReRender2 :: ThreadPostRequest -> Store -> HTMLView_
viewReRender2 !tpr' !store' =
  defineViewWithSKey "experiments-re-render-2" (tpr', store') go
  where
  go :: (ThreadPostRequest, Store) -> HTMLView_
  go (tpr, store@Store{..}) = do
    testIFrame
    cldiv_ B.well $ do
      textarea_ [ className_ B.formControl
                , "rows" $= "10"
                , "value" @= body
                , onChange $ \input -> dispatch $ ThreadPost.setBody tpr (PostDataRaw $ targetValue input)
                ] mempty
    where
    ThreadPostRequest{..} = tpr
    body                  = ThreadPost.postDataToBody threadPostRequestBody



viewReRender3 :: ThreadPostRequest -> Store -> Bool -> Bool -> HTMLView_ -> HTMLView_
viewReRender3 !tpr' !store' !tf1' !tf2' !plumbing' = do
  defineViewWithSKey "experiments-re-render-3" (tpr', store', tf1', tf2', plumbing') go
  where
  go (tpr, store, tf1, tf2, plumbing) = do
    plumbing
    viewReRender2 tpr store



viewReRender4
  :: ThreadPostRequest
  -> Store
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> HTMLView_
  -> HTMLView_
viewReRender4 !tpr' !store' !i1 !i2 !i3 !i4 !i5 !i6 !i7 !i8 !i9 !i10 !i11 !i12 !i13 !i14 !i15 !i16 !i17 !i18 !i19 !plumbing' = do
  defineViewWithSKey "experiments-re-render-4" (tpr', store', i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, plumbing') go
  where
  go (tpr, store, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, plumbing) = do
    plumbing
    viewReRender2 tpr store



viewReRender5 :: ThreadPostRequest -> Store -> HTMLView_ -> HTMLView_
viewReRender5 !tpr' !store' !plumbing' = do
  Loader.maybeLoader1 (Loaded $ Just tpr') $ \tpr -> do
    defineViewWithSKey "experiments-re-render-5" (tpr', store', plumbing') go
    where
    go (tpr, store, plumbing) = do
      plumbing
      viewReRender2 tpr store



testIFrame :: HTMLView_
testIFrame = do
  iframe_ [ "src" $= "https://www.youtube.com/embed/AVWRQ21Iorc", "height" @= (405 :: Int), "width" @= (720 :: Int) ] mempty
