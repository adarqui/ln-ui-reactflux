{-# LANGUAGE OverloadedStrings #-}

module LN.UI.View.Button (
  createButtonsCreateEditCancel,
  glyphButton,
  btn,
  btnXs,
  btnSm,
  btnMd,
  btnLg,
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
