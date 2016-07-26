{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LN.UI.View.Internal (
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
  showTagsSmall
) where


import           Data.Text                  (Text)
import           React.Flux                 hiding (view)
import qualified React.Flux                 as RF
import           React.Flux.Internal

import           LN.UI.Helpers.DataList     (toSeqList)
import           LN.UI.Helpers.GHCJS        (JSString, textToJSString')
import           LN.UI.Helpers.ReactFluxDOM (targetValue)
import           LN.UI.Types                (HTMLView_)



--
-- Buttons
--

buttonInfoClasses :: [(Text, Bool)]
buttonInfoClasses = buttonClasses "btn-info"

buttonDangerClasses :: [(Text, Bool)]
buttonDangerClasses = buttonClasses "btn-danger"

buttonClasses :: Text -> [(Text, Bool)]
buttonClasses button_type =  [("btn", True), (button_type, True)]



--
-- Forms
--

formGroupClasses :: [(Text, Bool)]
formGroupClasses = [("form-group", True)]

formControlClasses :: [(Text, Bool)]
formControlClasses = [("form-control", True)]



--
-- Radio
--

radioInlineClasses :: [(Text, Bool)]
radioInlineClasses = [("radio-inline", True)]



--
-- Widgets
--

createLabelInput :: Text -> Text -> Text -> PropertyOrHandler ViewEventHandler -> HTMLView_
createLabelInput label placeholder value handler =
  div_ [ classNames formGroupClasses
       ] $ do
    label_ [] $ elemText label
    input_ [ classNames formControlClasses
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
  cldiv_ "label-button-textarea input-group" $ do
    label_ [] $ elemText label
    textarea_ [ classNames formControlClasses
              , "placeholder" $= textToJSString' placeholder
              , "value"       $= textToJSString' value
              , value_change_handler
              ] mempty
    span_ [ "className" $= "input-group-btn" ] $ do
      button_ [ classNames buttonInfoClasses
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
      label_ [ classNames radioInlineClasses ] $ elemShow radio
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
  cldiv_ "input-group" $ do
    label_ $ elemText label
    input_ [ classNames formControlClasses
           , "value" $= textToJSString' current_tag
           , onChange (set_tag_handler . targetValue)
           , onKeyUp (\_ KeyboardEvent{..} -> if keyCode == 13 then add_tag_handler else mempty)
           ]
    span_ [ "className" $= "input-group-btn"
          , "title" $= "Add"
          , onClick (\_ _ -> add_tag_handler)
          ] $ elemText "Add"

  div_ $ do
    mapM_ (\(idx, tag) -> do
      span_ [ "classNames" $= "label label-default" ] $ elemText tag
      span_ [] $ do
        button_ [ "classNames" $= "btn btn-default btn-xs"
                , onClick (\_ _ -> delete_tag_handler idx)
                ] $ span_ [ "classNames" $= "glyphicon glyphicon-remove" ] $ elemText "✖"
      ) $ toSeqList tags




-- | creates a simple info button
--
-- simpleInfoButton "Create!" CreateResource
--
createSimpleInfoButton :: Text -> ViewEventHandler -> HTMLView_
createSimpleInfoButton label click_handler = do
  button_ [ classNames buttonInfoClasses
          , onClick $ \_ _ -> click_handler
          ] $ elemText label
--  H.p_ [ H.button [buttonInfoClasses, E.onClick (E.input_ act) ] [ H.text label ] ]



showTags :: [Text] -> HTMLView_
showTags tags =
  mapM_ (\tag -> do
    span_ [ "className" $= "label label-default" ] $ do
      elemText tag
    elemText " "
  ) tags



showTagsSmall :: [Text] -> HTMLView_
showTagsSmall tags = do
  small_ $ do
    span_ $ showTags tags
