module LN.View.Helpers (
  buttons_CreateEditCancel,

  glyphButton,

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
  glyphButtonLinkLg_Remove,

  buttonGroup_Horizontal,
  buttonGroup_Vertical,

  buttonGroup_HorizontalSm1,
  buttonGroup_VerticalSm1,

  buttonGroup_Horizontal',
  buttonGroup_Vertical',

  linkBadge,
  linkBadge',

  showBadge,
  showBadge',

  showTags,
  showTagsSmall
) where



import Data.Array                      (concat)
import Data.Maybe                      (Maybe(..), maybe)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Events.Indexed     as E
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3       as B
import Prelude                         (id, show, map, ($), (<>))

import LN.Halogen.Util
import LN.Input.Types                  (Input(..))
import LN.Router.Link                  (linkToHref)



buttons_CreateEditCancel m_edit_id save edit cancel =
  H.div_ [
    save_or_edit,
    simpleInfoButton "Cancel" (Goto cancel)
  ]
  where
  save_or_edit =
    case m_edit_id of
         Nothing      -> simpleInfoButton "Create" save
         Just edit_id -> simpleInfoButton "Save" (edit edit_id)



glyphButton glyph sz attrs handler =
--  H.span [P.class_ B.inputGroupBtn] [
    H.button [
      attrs,
--      P.alt alt,
      P.classes [B.btn, B.btnDefault, sz],
      E.onClick $ E.input_ handler
    ] [H.span [P.classes [B.glyphicon, glyph]] []]
--  ]



glyphButton_ArrowUp   = glyphButton B.glyphiconArrowUp
glyphButton_ArrowDown = glyphButton B.glyphiconArrowDown
glyphButton_Plus      = glyphButton B.glyphiconPlus
glyphButton_Minus     = glyphButton B.glyphiconMinus
glyphButton_Star      = glyphButton B.glyphiconStar
glyphButton_StarEmpty = glyphButton B.glyphiconStarEmpty
glyphButton_Trash     = glyphButton B.glyphiconTrash
glyphButton_Pencil    = glyphButton B.glyphiconPencil
glyphButton_Ok        = glyphButton B.glyphiconOk
glyphButton_Remove    = glyphButton B.glyphiconRemove



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



glyphButtonSm_ArrowUp   = glyphButton_ArrowUp B.btnSm
glyphButtonSm_ArrowDown = glyphButton_ArrowDown B.btnSm
glyphButtonSm_Plus      = glyphButton_Plus B.btnSm
glyphButtonSm_Minus     = glyphButton_Minus B.btnSm
glyphButtonSm_Star      = glyphButton_Star B.btnSm
glyphButtonSm_StarEmpty = glyphButton_StarEmpty B.btnSm
glyphButtonSm_Trash     = glyphButton_Trash B.btnSm
glyphButtonSm_Pencil    = glyphButton_Pencil B.btnSm
glyphButtonSm_Ok        = glyphButton_Ok B.btnSm
glyphButtonSm_Remove    = glyphButton_Remove B.btnSm



glyphButtonLg_ArrowUp   = glyphButton_ArrowUp B.btnLg
glyphButtonLg_ArrowDown = glyphButton_ArrowDown B.btnLg
glyphButtonLg_Plus      = glyphButton_Plus B.btnLg
glyphButtonLg_Minus     = glyphButton_Minus B.btnLg
glyphButtonLg_Star      = glyphButton_Star B.btnLg
glyphButtonLg_StarEmpty = glyphButton_StarEmpty B.btnLg
glyphButtonLg_Trash     = glyphButton_Trash B.btnLg
glyphButtonLg_Pencil    = glyphButton_Pencil B.btnLg
glyphButtonLg_Ok        = glyphButton_Ok B.btnLg
glyphButtonLg_Remove    = glyphButton_Remove B.btnLg



-- MAN WTF DO MY GLYPH ICONS KEEP VIBRATING?
--
-- glyphButtonLink glyph sz params link =
--  H.span [P.class_ B.inputGroupBtn] [
--    H.a [
--      linkToHref link,
--      P.classes [B.btn, B.btnDefault, sz]
--    ] [H.span [P.classes [B.glyphicon, glyph]] []]
--  ]

glyphButtonLink glyph sz link =
  glyphButton glyph sz (P.classes []) $ Goto link



glyphButtonLink_ArrowUp   = glyphButtonLink B.glyphiconArrowUp
glyphButtonLink_ArrowDown = glyphButtonLink B.glyphiconArrowDown
glyphButtonLink_Plus      = glyphButtonLink B.glyphiconPlus
glyphButtonLink_Minus     = glyphButtonLink B.glyphiconMinus
glyphButtonLink_Star      = glyphButtonLink B.glyphiconStar
glyphButtonLink_StarEmpty = glyphButtonLink B.glyphiconStarEmpty
glyphButtonLink_Trash     = glyphButtonLink B.glyphiconTrash
glyphButtonLink_Pencil    = glyphButtonLink B.glyphiconPencil
glyphButtonLink_Ok        = glyphButtonLink B.glyphiconOk
glyphButtonLink_Remove    = glyphButtonLink B.glyphiconRemove



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



glyphButtonLinkSm_ArrowUp   = glyphButtonLink_ArrowUp B.btnSm
glyphButtonLinkSm_ArrowDown = glyphButtonLink_ArrowDown B.btnSm
glyphButtonLinkSm_Plus      = glyphButtonLink_Plus B.btnSm
glyphButtonLinkSm_Minus     = glyphButtonLink_Minus B.btnSm
glyphButtonLinkSm_Star      = glyphButtonLink_Star B.btnSm
glyphButtonLinkSm_StarEmpty = glyphButtonLink_StarEmpty B.btnSm
glyphButtonLinkSm_Trash     = glyphButtonLink_Trash B.btnSm
glyphButtonLinkSm_Pencil    = glyphButtonLink_Pencil B.btnSm
glyphButtonLinkSm_Ok        = glyphButtonLink_Ok B.btnSm
glyphButtonLinkSm_Remove    = glyphButtonLink_Remove B.btnSm



glyphButtonLinkLg_ArrowUp   = glyphButtonLink_ArrowUp B.btnLg
glyphButtonLinkLg_ArrowDown = glyphButtonLink_ArrowDown B.btnLg
glyphButtonLinkLg_Plus      = glyphButtonLink_Plus B.btnLg
glyphButtonLinkLg_Minus     = glyphButtonLink_Minus B.btnLg
glyphButtonLinkLg_Star      = glyphButtonLink_Star B.btnLg
glyphButtonLinkLg_StarEmpty = glyphButtonLink_StarEmpty B.btnLg
glyphButtonLinkLg_Trash     = glyphButtonLink_Trash B.btnLg
glyphButtonLinkLg_Pencil    = glyphButtonLink_Pencil B.btnLg
glyphButtonLinkLg_Ok        = glyphButtonLink_Ok B.btnLg
glyphButtonLinkLg_Remove    = glyphButtonLink_Remove B.btnLg



buttonGroup_Horizontal xs = H.div_ xs
buttonGroup_Vertical xs   = H.div_ xs



buttonGroup_HorizontalSm1 = buttonGroup_Horizontal' B.colSm1
buttonGroup_VerticalSm1 = buttonGroup_Vertical' B.colSm1



buttonGroup_Horizontal' sz xs =
  H.div [P.classes [B.btnGroup]] xs

buttonGroup_Vertical' sz xs =
  H.div [P.classes [B.btnGroupVertical]] xs




linkBadge attrs text badge route =
  H.a [
    linkToHref route,
    attrs
  ] [H.text text, H.span [P.classes [B.badge]] [H.text badge]]



linkBadge' = linkBadge (P.classes [])



showBadge attrs text badge =
  H.p_ [H.text text, H.span [P.classes [B.badge]] [H.text $ show badge]]



showBadge' = showBadge (P.classes [])



showTags tags =
  concat $ map (\tag ->
      [
        H.span [P.classes [B.label, B.labelDefault]] [
          H.text tag
        ],
        H.text " "
      ]
  ) tags



showTagsSmall tags = H.small_ [H.span_ $ showTags tags]
