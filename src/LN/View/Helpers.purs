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

  glyphButtonDef_ArrowUp,
  glyphButtonDef_ArrowDown,
  glyphButtonDef_Plus,
  glyphButtonDef_Minus,
  glyphButtonDef_Star,
  glyphButtonDef_StarEmpty,
  glyphButtonDef_Trash,
  glyphButtonDef_Pencil,

  glyphButtonSm_ArrowUp,
  glyphButtonSm_ArrowDown,
  glyphButtonSm_Plus,
  glyphButtonSm_Minus,
  glyphButtonSm_Star,
  glyphButtonSm_StarEmpty,
  glyphButtonSm_Trash,
  glyphButtonSm_Pencil,

  glyphButtonLg_ArrowUp,
  glyphButtonLg_ArrowDown,
  glyphButtonLg_Plus,
  glyphButtonLg_Minus,
  glyphButtonLg_Star,
  glyphButtonLg_StarEmpty,
  glyphButtonLg_Trash,
  glyphButtonLg_Pencil,

  buttonGroup_Horizontal,
  buttonGroup_Vertical
) where



import Daimyo.Data.ArrayList           (listToArray)
import Data.Map                        as M
import Data.Maybe                      (Maybe(..), maybe)
import Halogen                         (ComponentHTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Events.Indexed     as E
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3       as B
import Optic.Core                      ((^.), (..))
import Prelude                         (id, map, ($), (<>))

import LN.Halogen.Util
import LN.Input.Types                  (Input(..))
import LN.Router.Link                  (linkToP_Classes, linkToP_Glyph', linkToP)
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.State.Loading                (getLoading, l_currentOrganization)
import LN.State.Types                  (State)
import LN.View.Module.Loading          (renderLoading)
import LN.View.Forums.Show             (renderView_Forums_Show)
import LN.T                            ( OrganizationPackResponse
                                       , _OrganizationPackResponse, _OrganizationResponse, organization_
                                       , _ForumPackResponse, _ForumResponse, forum_)



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



glyphButton glyph sz attrs alt handler =
  H.span [P.class_ B.inputGroupBtn] [
    H.button [
      attrs,
--      P.alt alt,
      P.classes [B.btn, B.btnDefault, sz],
      E.onClick $ E.input_ handler
    ] [H.span [P.classes [B.glyphicon, glyph]] []]
  ]



glyphButton_ArrowUp   = glyphButton B.glyphiconArrowUp
glyphButton_ArrowDown = glyphButton B.glyphiconArrowDown
glyphButton_Plus      = glyphButton B.glyphiconPlus
glyphButton_Minus     = glyphButton B.glyphiconMinus
glyphButton_Star      = glyphButton B.glyphiconStar
glyphButton_StarEmpty = glyphButton B.glyphiconStarEmpty
glyphButton_Trash     = glyphButton B.glyphiconTrash
glyphButton_Pencil    = glyphButton B.glyphiconPencil



glyphButtonDef_ArrowUp   = glyphButtonSm_ArrowUp
glyphButtonDef_ArrowDown = glyphButtonSm_ArrowDown
glyphButtonDef_Plus      = glyphButtonSm_Plus
glyphButtonDef_Minus     = glyphButtonSm_Minus
glyphButtonDef_Star      = glyphButtonSm_Star
glyphButtonDef_StarEmpty = glyphButtonSm_StarEmpty
glyphButtonDef_Trash     = glyphButtonSm_Trash
glyphButtonDef_Pencil    = glyphButtonSm_Pencil



glyphButtonSm_ArrowUp   = glyphButton_ArrowUp B.btnSm
glyphButtonSm_ArrowDown = glyphButton_ArrowDown B.btnSm
glyphButtonSm_Plus      = glyphButton_Plus B.btnSm
glyphButtonSm_Minus     = glyphButton_Minus B.btnSm
glyphButtonSm_Star      = glyphButton_Star B.btnSm
glyphButtonSm_StarEmpty = glyphButton_StarEmpty B.btnSm
glyphButtonSm_Trash     = glyphButton_Trash B.btnSm
glyphButtonSm_Pencil    = glyphButton_Pencil B.btnSm



glyphButtonLg_ArrowUp   = glyphButton_ArrowUp B.btnLg
glyphButtonLg_ArrowDown = glyphButton_ArrowDown B.btnLg
glyphButtonLg_Plus      = glyphButton_Plus B.btnLg
glyphButtonLg_Minus     = glyphButton_Minus B.btnLg
glyphButtonLg_Star      = glyphButton_Star B.btnLg
glyphButtonLg_StarEmpty = glyphButton_StarEmpty B.btnLg
glyphButtonLg_Trash     = glyphButton_Trash B.btnLg
glyphButtonLg_Pencil    = glyphButton_Pencil B.btnLg



buttonGroup_Horizontal = []
buttonGroup_Vertical = []
