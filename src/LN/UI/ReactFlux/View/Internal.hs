{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LN.UI.ReactFlux.View.Internal (
  buttonInfoClasses,
  buttonDangerClasses,
  buttonClasses,
  formGroupClasses,
  formControlClasses,
  radioInlineClasses,
  createLabelInput,
  createLabelButtonTextArea,
  createRadioMenu,
  createTagsField,
  createSimpleInfoButton,
  showTags,
  showTagsSmall,
  showTable,
  showTableClean
) where


import           Control.Monad                        (forM_)
import           Data.Text                            (Text)
import           React.Flux                           hiding (view)
import qualified React.Flux                           as RF
import           React.Flux.Internal hiding (JSString)
import qualified Web.Bootstrap3                       as B

import           LN.UI.Core.Helpers.DataList          (toSeqList)
import           LN.UI.Core.Helpers.GHCJS             (JSString,
                                                       textToJSString')
import           LN.UI.ReactFlux.Helpers.ReactFluxDOM (className_, classNames_,
                                                       targetValue)
import           LN.UI.ReactFlux.Types                (HTMLView_)



--
-- Buttons
--

buttonInfoClasses :: [Text]
buttonInfoClasses = buttonClasses B.btnInfo

buttonDangerClasses :: [Text]
buttonDangerClasses = buttonClasses B.btnDanger

buttonClasses :: Text -> [Text]
buttonClasses button_type =  [B.btn, button_type]



--
-- Forms
--

formGroupClasses :: [Text]
formGroupClasses = [B.formGroup]

formControlClasses :: [Text]
formControlClasses = [B.formControl]



--
-- Radio
--

radioInlineClasses :: [Text]
radioInlineClasses = [B.radioInline]



--
-- Widgets
--

createLabelInput :: Text -> Text -> Text -> PropertyOrHandler ViewEventHandler -> HTMLView_
createLabelInput label placeholder value handler =
  div_ [ classNames_ formGroupClasses
       ] $ do
    label_ [] $ elemText label
    input_ [ classNames_ formControlClasses
           , "type"        $= "text"
           , "placeholder" $= textToJSString' placeholder
           , "value"       $= textToJSString' value
           , handler
           ]



createLabelButtonTextArea
  :: Text
  -> Text
  -> Text
  -> Text
  -> PropertyOrHandler ViewEventHandler
  -> PropertyOrHandler ViewEventHandler
  -> HTMLView_

createLabelButtonTextArea label placeholder value button_name value_change_handler button_handler =
  div_ [classNames_ ["label-button-textarea", B.inputGroup]] $ do
    label_ [] $ elemText label
    textarea_ [ classNames_ formControlClasses
              , "placeholder" $= textToJSString' placeholder
              , "value"       $= textToJSString' value
              , value_change_handler
              ] mempty
    span_ [className_ B.inputGroupBtn] $ do
      button_ [ classNames_ buttonInfoClasses
              , "title" $= textToJSString' button_name
              , button_handler
              ] $ elemText button_name



-- | Creates a radio menu
--
-- radioMenu "Leuron Type" "leuron-type" [NONE, FACT, CARD] SetLeuronType
--
createRadioMenu
  :: forall a radio. (Show radio, Eq radio)
  => Text
  -> Text
  -> [radio]
  -> radio
  -> (radio -> ViewEventHandler)
  -> HTMLView_

createRadioMenu menu_label radio_name radios checked_value radio_handler =
  p_ $ do
    label_ $ elemText menu_label
    mapM_ (\radio -> do
      label_ [ classNames_ radioInlineClasses ] $ elemShow radio
      input_ [ "type"    $= "radio"
             , "name"    $= textToJSString' radio_name
             , "checked" @= (checked_value == radio)
             , onChange (\_ -> radio_handler radio)
             ]
      ) radios



-- | Generic "Tags" function
-- label: Tags, Private Tags
-- tags: Array of tag strings
-- current_tag: current string which may be added as a tag
-- set_tag: set st.current_tag, which is what we have been editing
-- add_tag: add tag to st.tags
-- delete_tag: delete tag by index
-- clear_tags: remove all tags
--
createTagsField
  :: Text
  -> [Text]
  -> Text
  -> (Text -> ViewEventHandler) -- ^ set current tag
  -> ViewEventHandler           -- ^ add current tag
  -> (Int -> ViewEventHandler)  -- ^ delete tag
  -> ViewEventHandler           -- ^ clear tags
  -> HTMLView_

createTagsField label tags current_tag set_tag_handler add_tag_handler delete_tag_handler clear_tags_handler = do
  cldiv_ B.inputGroup $ do
    label_ $ elemText label
    input_ [ classNames_ formControlClasses
           , "value" $= textToJSString' current_tag
           , onChange (set_tag_handler . targetValue)
           , onKeyUp (\_ KeyboardEvent{..} -> if keyCode == 13 then add_tag_handler else mempty)
           ]
    span_ [ className_ B.inputGroupBtn
          , "title" $= "Add"
          , onClick (\_ _ -> add_tag_handler)
          ] $ elemText "Add"

  div_ $ do
    mapM_ (\(idx, tag) -> do
      span_ [classNames_ [B.label, B.labelDefault]] $ elemText tag
      span_ [] $ do
        button_ [ classNames_ [B.btn, B.btnDefault, B.btnXs]
                , onClick (\_ _ -> delete_tag_handler idx)
                ] $ span_ [classNames_ [B.glyphicon, B.glyphiconRemove]] mempty
      ) $ toSeqList tags




-- | creates a simple info button
--
-- simpleInfoButton "Create!" CreateResource
--
createSimpleInfoButton :: Text -> ViewEventHandler -> HTMLView_
createSimpleInfoButton label click_handler = do
  button_ [ classNames_ buttonInfoClasses
          , onClick $ \_ _ -> click_handler
          ] $ elemText label
--  H.p_ [ H.button [buttonInfoClasses, E.onClick (E.input_ act) ] [ H.text label ] ]



showTags :: [Text] -> HTMLView_
showTags tags =
  mapM_ (\tag -> do
    span_ [classNames_ [B.label, B.labelDefault]] $ do
      elemText tag
    elemText " "
  ) tags



showTagsSmall :: [Text] -> HTMLView_
showTagsSmall tags = do
  small_ $ do
    span_ $ showTags tags



-- | Creates a simple table like so:
--
--
showTable
  :: Show a
  => [Text]
  -> [Text]
  -> [Text]
  -> [[a]]
  -> HTMLView_

showTable attrs column_headings row_headings rows = do
  table_ [classNames_ attrs] $ do
    thead_ $ do
      forM_ column_headings $ th_ . elemText
    tbody_ $ do
      forM_ (zip row_headings rows) $ \(row_heading, row_values) -> do
        tr_ $ do
          th_ $ elemText row_heading
          forM_ row_values $ td_ . elemShow



showTableClean
  :: Show a
  => [Text]
  -> [Text]
  -> [[a]]
  -> HTMLView_

showTableClean = showTable [B.table, B.tableStriped, B.tableCondensed]
