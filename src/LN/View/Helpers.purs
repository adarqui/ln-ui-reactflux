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

  button_newOrganization,
  button_newOrganization,
  button_editOrganization,
  button_deleteOrganization,
  button_joinOrganization,
  button_newForum,
  button_editForum,
  button_deleteForum,
  button_newBoard,
  button_editBoard,
  button_deleteBoard,
  button_newThread,
  button_editThread,
  button_deleteThread,
  button_newThreadPost,
  button_editThreadPost,
  button_deleteThreadPost,
  button_like,
  button_neutral,
  button_dislike,
  button_starEmpty,
  button_star,

  linkBadge,
  linkBadge',

  showBadge,
  showBadge',

  showTags,
  showTagsSmall,

  textButton,
  textButtonSm,
  textButtonLg
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
import LN.Router.Class.Routes          (Routes(..))



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



glyphButton glyph sz m_text attrs handler =
  H.button [
    attrs,
    P.classes [B.btn, B.btnDefault, sz],
    E.onClick $ E.input_ handler
  ] [H.span [P.classes [B.glyphicon, glyph]] text]
  where
  text =
    case m_text of
      Nothing   -> []
      Just text -> [H.text text]



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

glyphButtonLink glyph sz m_text link =
  glyphButton glyph sz m_text (P.classes []) $ Goto link



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




textButton sz label handler =
    H.button [
      P.classes [B.btn, B.btnDefault, sz],
      E.onClick $ E.input_ handler
    ] [H.span [] [H.text label]]

textButtonSm = textButton B.btnSm
textButtonLg = textButton B.btnLg
