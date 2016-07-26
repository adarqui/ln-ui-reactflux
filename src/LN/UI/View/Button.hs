{-# LANGUAGE OverloadedStrings #-}

module LN.UI.View.Button (
  createButtonsCreateEditCancel,
  glyphButton,
  btn,
  btnXs,
  btnSm,
  btnMd,
  btnLg,
btnGroup,
btnGroupVertical,
colSm1,
glyphicon, glyphiconArrowUp, glyphiconArrowDown, glyphiconPlus, glyphiconMinus, glyphiconStar, glyphiconStarEmpty, glyphiconTrash, glyphiconPencil, glyphiconOk, glyphiconRemove ,
 glyphButton_ArrowUp,
 glyphButton_ArrowDown,
 glyphButton_Plus,
 glyphButton_Minus,
 glyphButton_Star,
 glyphButton_StarEmpty,
 glyphButton_Trash,
 glyphButton_Pencil,
 glyphButton_Ok,
 glyphButton_Remove,

 glyphButtonDef_ArrowUp,
 glyphButtonDef_ArrowDown,
 glyphButtonDef_Plus,
 glyphButtonDef_Minus,
 glyphButtonDef_Star,
 glyphButtonDef_StarEmpty,
 glyphButtonDef_Trash,
 glyphButtonDef_Pencil,
 glyphButtonDef_Ok,
 glyphButtonDef_Remove,

 glyphButtonSm_ArrowUp,
 glyphButtonSm_ArrowDown,
 glyphButtonSm_Plus,
 glyphButtonSm_Minus,
 glyphButtonSm_Star,
 glyphButtonSm_StarEmpty,
 glyphButtonSm_Trash,
 glyphButtonSm_Pencil,
 glyphButtonSm_Ok,
 glyphButtonSm_Remove,

 glyphButtonLg_ArrowUp,
 glyphButtonLg_ArrowDown,
 glyphButtonLg_Plus,
 glyphButtonLg_Minus,
 glyphButtonLg_Star,
 glyphButtonLg_StarEmpty,
 glyphButtonLg_Trash,
 glyphButtonLg_Pencil,
 glyphButtonLg_Ok,
 glyphButtonLg_Remove,

 glyphButtonLink,

 glyphButtonLink_ArrowUp,
 glyphButtonLink_ArrowDown,
 glyphButtonLink_Plus,
 glyphButtonLink_Minus,
 glyphButtonLink_Star,
 glyphButtonLink_StarEmpty,
 glyphButtonLink_Trash,
 glyphButtonLink_Pencil,
 glyphButtonLink_Ok,
 glyphButtonLink_Remove,

 glyphButtonLinkDef_ArrowUp,
 glyphButtonLinkDef_ArrowDown,
 glyphButtonLinkDef_Plus,
 glyphButtonLinkDef_Minus,
 glyphButtonLinkDef_Star,
 glyphButtonLinkDef_StarEmpty,
 glyphButtonLinkDef_Trash,
 glyphButtonLinkDef_Pencil,
 glyphButtonLinkDef_Ok,
 glyphButtonLinkDef_Remove,

 glyphButtonLinkSm_ArrowUp,
 glyphButtonLinkSm_ArrowDown,
 glyphButtonLinkSm_Plus,
 glyphButtonLinkSm_Minus,
 glyphButtonLinkSm_Star,
 glyphButtonLinkSm_StarEmpty,
 glyphButtonLinkSm_Trash,
 glyphButtonLinkSm_Pencil,
 glyphButtonLinkSm_Ok,
 glyphButtonLinkSm_Remove,

 glyphButtonLinkLg_ArrowUp,
 glyphButtonLinkLg_ArrowDown,
 glyphButtonLinkLg_Plus,
 glyphButtonLinkLg_Minus,
 glyphButtonLinkLg_Star,
 glyphButtonLinkLg_StarEmpty,
 glyphButtonLinkLg_Trash,
 glyphButtonLinkLg_Pencil,
 glyphButtonLinkLg_Ok,
 glyphButtonLinkLg_Remove
) where



import Data.Monoid ((<>))
import           Data.Int            (Int64)
import Data.Text (Text)
import           React.Flux

import qualified LN.UI.App.Route     as Route (Action (..), dispatch)
import           LN.UI.Router.Route  (RouteWith (..))
import           LN.UI.Types         (HTMLView_)
import           LN.UI.View.Internal
import LN.UI.Helpers.ReactFluxDOM (ahrefElement)
import LN.UI.Helpers.GHCJS (JSString)



createButtonsCreateEditCancel
  :: Maybe Int64
  -> ViewEventHandler -- ^ save handler
  -> (Int64 -> ViewEventHandler) -- ^ edit handler
  -> RouteWith -- ^ cancel route
  -> HTMLView_

createButtonsCreateEditCancel m_edit_id save_handler edit_handler cancel_route_with =
  div_ $ do
    save_or_edit
    createSimpleInfoButton "Cancel" (Route.dispatch $ Route.Goto cancel_route_with)
  where
  save_or_edit =
    case m_edit_id of
      Nothing      -> createSimpleInfoButton "Create" save_handler
      Just edit_id -> createSimpleInfoButton "Save" (edit_handler edit_id)



glyphButton :: JSString -> JSString -> Maybe Text -> [SomeStoreAction] -> HTMLView_
glyphButton glyph sz m_text click_handler =
  button_ [ "className" $= ("btn btn-default " <> sz)
          , onClick $ \_ _ -> click_handler
          ] $ do
            span_ [ "className" $= ("glyphicon " <> glyph)] text
  where
  text =
    case m_text of
      Nothing   -> mempty
      Just text -> elemText text



btn, btnXs, btnSm, btnMd, btnLg :: JSString
btn   = "btn"
btnXs = "btn-xs"
btnSm = "btn-sm"
btnMd = "btn-md"
btnLg = "btn-lg"



btnGroup, btnGroupVertical :: JSString
btnGroup = "btn-group"
btnGroupVertical = "btn-group-vertical"



colSm1 :: JSString
colSm1 = "col-sm-1"



glyphicon, glyphiconArrowUp, glyphiconArrowDown, glyphiconPlus, glyphiconMinus, glyphiconStar, glyphiconStarEmpty, glyphiconTrash, glyphiconPencil, glyphiconOk, glyphiconRemove :: JSString
glyphicon           =  "glyphicon"
glyphiconArrowUp    =  "glyphicon-arrow-up"
glyphiconArrowDown  =  "glyphicon-arrow-down"
glyphiconPlus       =  "glyphicon-plus"
glyphiconMinus      =  "glyphicon-minus"
glyphiconStar       =  "glyphicon-star"
glyphiconStarEmpty  =  "glyphicon-star-empty"
glyphiconTrash      =  "glyphicon-trash"
glyphiconPencil     =  "glyphicon-pencil"
glyphiconOk         =  "glyphicon-ok"
glyphiconRemove     =  "glyphicon-remove"



glyphButton_ArrowUp   = glyphButton glyphiconArrowUp
glyphButton_ArrowDown = glyphButton glyphiconArrowDown
glyphButton_Plus      = glyphButton glyphiconPlus
glyphButton_Minus     = glyphButton glyphiconMinus
glyphButton_Star      = glyphButton glyphiconStar
glyphButton_StarEmpty = glyphButton glyphiconStarEmpty
glyphButton_Trash     = glyphButton glyphiconTrash
glyphButton_Pencil    = glyphButton glyphiconPencil
glyphButton_Ok        = glyphButton glyphiconOk
glyphButton_Remove    = glyphButton glyphiconRemove



glyphButtonDef_ArrowUp   = glyphButtonSm_ArrowUp
glyphButtonDef_ArrowDown = glyphButtonSm_ArrowDown
glyphButtonDef_Plus      = glyphButtonSm_Plus
glyphButtonDef_Minus     = glyphButtonSm_Minus
glyphButtonDef_Star      = glyphButtonSm_Star
glyphButtonDef_StarEmpty = glyphButtonSm_StarEmpty
glyphButtonDef_Trash     = glyphButtonSm_Trash
glyphButtonDef_Pencil    = glyphButtonSm_Pencil
glyphButtonDef_Ok        = glyphButtonSm_Ok
glyphButtonDef_Remove    = glyphButtonSm_Remove



glyphButtonSm_ArrowUp   = glyphButton_ArrowUp btnSm
glyphButtonSm_ArrowDown = glyphButton_ArrowDown btnSm
glyphButtonSm_Plus      = glyphButton_Plus btnSm
glyphButtonSm_Minus     = glyphButton_Minus btnSm
glyphButtonSm_Star      = glyphButton_Star btnSm
glyphButtonSm_StarEmpty = glyphButton_StarEmpty btnSm
glyphButtonSm_Trash     = glyphButton_Trash btnSm
glyphButtonSm_Pencil    = glyphButton_Pencil btnSm
glyphButtonSm_Ok        = glyphButton_Ok btnSm
glyphButtonSm_Remove    = glyphButton_Remove btnSm



glyphButtonLg_ArrowUp   = glyphButton_ArrowUp btnLg
glyphButtonLg_ArrowDown = glyphButton_ArrowDown btnLg
glyphButtonLg_Plus      = glyphButton_Plus btnLg
glyphButtonLg_Minus     = glyphButton_Minus btnLg
glyphButtonLg_Star      = glyphButton_Star btnLg
glyphButtonLg_StarEmpty = glyphButton_StarEmpty btnLg
glyphButtonLg_Trash     = glyphButton_Trash btnLg
glyphButtonLg_Pencil    = glyphButton_Pencil btnLg
glyphButtonLg_Ok        = glyphButton_Ok btnLg
glyphButtonLg_Remove    = glyphButton_Remove btnLg



glyphButtonLink :: JSString -> JSString -> Maybe Text -> RouteWith-> HTMLView_
glyphButtonLink glyph sz m_text route_with =
  glyphButton glyph sz m_text $ Route.dispatch $ Route.Goto route_with



glyphButtonLink_ArrowUp   = glyphButtonLink glyphiconArrowUp
glyphButtonLink_ArrowDown = glyphButtonLink glyphiconArrowDown
glyphButtonLink_Plus      = glyphButtonLink glyphiconPlus
glyphButtonLink_Minus     = glyphButtonLink glyphiconMinus
glyphButtonLink_Star      = glyphButtonLink glyphiconStar
glyphButtonLink_StarEmpty = glyphButtonLink glyphiconStarEmpty
glyphButtonLink_Trash     = glyphButtonLink glyphiconTrash
glyphButtonLink_Pencil    = glyphButtonLink glyphiconPencil
glyphButtonLink_Ok        = glyphButtonLink glyphiconOk
glyphButtonLink_Remove    = glyphButtonLink glyphiconRemove



glyphButtonLinkDef_ArrowUp   = glyphButtonLinkSm_ArrowUp
glyphButtonLinkDef_ArrowDown = glyphButtonLinkSm_ArrowDown
glyphButtonLinkDef_Plus      = glyphButtonLinkSm_Plus
glyphButtonLinkDef_Minus     = glyphButtonLinkSm_Minus
glyphButtonLinkDef_Star      = glyphButtonLinkSm_Star
glyphButtonLinkDef_StarEmpty = glyphButtonLinkSm_StarEmpty
glyphButtonLinkDef_Trash     = glyphButtonLinkSm_Trash
glyphButtonLinkDef_Pencil    = glyphButtonLinkSm_Pencil
glyphButtonLinkDef_Ok        = glyphButtonLinkSm_Ok
glyphButtonLinkDef_Remove    = glyphButtonLinkSm_Remove



glyphButtonLinkSm_ArrowUp   = glyphButtonLink_ArrowUp btnSm
glyphButtonLinkSm_ArrowDown = glyphButtonLink_ArrowDown btnSm
glyphButtonLinkSm_Plus      = glyphButtonLink_Plus btnSm
glyphButtonLinkSm_Minus     = glyphButtonLink_Minus btnSm
glyphButtonLinkSm_Star      = glyphButtonLink_Star btnSm
glyphButtonLinkSm_StarEmpty = glyphButtonLink_StarEmpty btnSm
glyphButtonLinkSm_Trash     = glyphButtonLink_Trash btnSm
glyphButtonLinkSm_Pencil    = glyphButtonLink_Pencil btnSm
glyphButtonLinkSm_Ok        = glyphButtonLink_Ok btnSm
glyphButtonLinkSm_Remove    = glyphButtonLink_Remove btnSm



glyphButtonLinkLg_ArrowUp   = glyphButtonLink_ArrowUp btnLg
glyphButtonLinkLg_ArrowDown = glyphButtonLink_ArrowDown btnLg
glyphButtonLinkLg_Plus      = glyphButtonLink_Plus btnLg
glyphButtonLinkLg_Minus     = glyphButtonLink_Minus btnLg
glyphButtonLinkLg_Star      = glyphButtonLink_Star btnLg
glyphButtonLinkLg_StarEmpty = glyphButtonLink_StarEmpty btnLg
glyphButtonLinkLg_Trash     = glyphButtonLink_Trash btnLg
glyphButtonLinkLg_Pencil    = glyphButtonLink_Pencil btnLg
glyphButtonLinkLg_Ok        = glyphButtonLink_Ok btnLg
glyphButtonLinkLg_Remove    = glyphButtonLink_Remove btnLg



buttonGroup_Horizontal, buttonGroup_Vertical :: HTMLView_ -> HTMLView_
buttonGroup_Horizontal xs = div_ xs
buttonGroup_Vertical xs   = div_ xs



buttonGroup_HorizontalSm1, buttonGroup_VerticalSm1 :: HTMLView_ -> HTMLView_
buttonGroup_HorizontalSm1 = buttonGroup_Horizontal' colSm1
buttonGroup_VerticalSm1 = buttonGroup_Vertical' colSm1



buttonGroup_Horizontal' :: JSString -> HTMLView_ -> HTMLView_
buttonGroup_Horizontal' sz xs =
  div_ [ "className" $= btnGroup ] xs

buttonGroup_Vertical' :: JSString -> HTMLView_ -> HTMLView_
buttonGroup_Vertical' sz xs =
  div_ [ "className" $= btnGroupVertical ] xs



button_newOrganization     = glyphButtonLinkDef_Plus (Just " new-organization")
button_editOrganization    = glyphButtonLinkDef_Pencil Nothing
button_editOrganization'   = glyphButtonLinkDef_Pencil (Just "edit organization")
button_deleteOrganization  = glyphButtonLinkDef_Trash Nothing
button_deleteOrganization' = glyphButtonLinkDef_Trash (Just "delete organization")
button_joinOrganization    = glyphButtonLinkDef_Ok (Just " join-organization")

button_newForum     = glyphButtonLinkDef_Plus (Just " new-forum")
button_editForum    = glyphButtonLinkDef_Pencil Nothing
button_editForum'   = glyphButtonLinkDef_Pencil (Just "edit forum")
button_deleteForum  = glyphButtonLinkDef_Trash Nothing
button_deleteForum' = glyphButtonLinkDef_Trash (Just "delete forum")

button_newBoard     = glyphButtonLinkDef_Plus (Just " new-board")
button_editBoard    = glyphButtonLinkDef_Pencil Nothing
button_editBoard'   = glyphButtonLinkDef_Pencil (Just "edit board")
button_deleteBoard  = glyphButtonLinkDef_Trash Nothing
button_deleteBoard' = glyphButtonLinkDef_Trash (Just "delete board")

button_newThread     = glyphButtonLinkDef_Plus (Just " new-thread")
button_editThread    = glyphButtonLinkDef_Pencil Nothing
button_editThread'   = glyphButtonLinkDef_Pencil (Just "edit thread")
button_deleteThread  = glyphButtonLinkDef_Trash Nothing
button_deleteThread' = glyphButtonLinkDef_Trash (Just "delete thread")

button_newThreadPost     = glyphButtonLinkDef_Plus (Just " new-thread-post")
button_editThreadPost    = glyphButtonLinkDef_Pencil Nothing
button_editThreadPost'   = glyphButtonLinkDef_Pencil (Just "edit thread post")
button_deleteThreadPost  = glyphButtonLinkDef_Trash Nothing
button_deleteThreadPost' = glyphButtonLinkDef_Trash (Just "delete thread post")

button_like classes handler    = glyphButtonDef_ArrowUp Nothing classes handler
button_neutral classes handler = glyphButtonDef_Minus Nothing classes handler
button_dislike classes handler = glyphButtonDef_ArrowDown Nothing classes handler

button_starEmpty classes handler = glyphButtonDef_StarEmpty Nothing classes handler
button_star classes handler      = glyphButtonDef_Star Nothing classes handler



linkBadge :: Text -> JSString -> RouteWith -> HTMLView_
linkBadge text badge route_with =
  ahrefElement route_with $ do
    elemText text
    span_ [ "className" $= badge ] $ elemText badge



showBadge :: Text -> JSString -> HTMLView_
showBadge text badge =
  p_ $ do
    elemText text
    span_ [ "className" $= badge ] $ elemText $ show badge



textButton :: JSString -> Text -> [SomeStoreAction] -> HTMLView_
textButton sz label handler =
  button_ [ "className" $= ("btn btn-default " <> sz)
          , onClick $ \_ _ -> handler
          ] $ do
            span_ $ elemText label

textButtonSm, textButtonLg :: Text -> [SomeStoreAction] -> HTMLView_
textButtonSm = textButton btnSm
textButtonLg = textButton btnLg
