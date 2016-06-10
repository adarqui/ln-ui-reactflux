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
  glyphButton_Pencil
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



glyphButton glyph attrs alt handler =
  H.span [P.class_ B.inputGroupBtn] [
    H.button [
      attrs,
--      P.alt alt,
      P.classes [B.btn, B.btnDefault, B.btnLg],
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
