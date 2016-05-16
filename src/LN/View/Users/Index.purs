module LN.View.Users.Index (
  renderView_Users_Index
) where



import Halogen                      (ComponentHTML)
import Optic.Core                   ((^.), (..))
import Prelude                      (map, ($))

import LN.Input.Types               (Input)
import LN.Router.Types              (Routes(..), CRUD(..))
import LN.State.Types               (State)
import LN.T
import LN.View.Module.Gravatar
import LN.View.Module.PageNumbers
import LN.View.Module.EntityListing



renderView_Users_Index :: State -> ComponentHTML Input
renderView_Users_Index st =
  renderEntityListing "Users" (
    map (\user ->
      { nick: user ^. _UserSanitizedPackResponse .. user_ ^. _UserSanitizedResponse .. nick_
      , displayNick: user ^. _UserSanitizedPackResponse .. user_ ^. _UserSanitizedResponse .. displayNick_
      , createdAt: user ^. _UserSanitizedPackResponse .. user_ ^. _UserSanitizedResponse .. createdAt_
      , logo: gravatarUrlFromUser XLarge (user ^. _UserSanitizedPackResponse .. user_)
      , route: Users (Show $ user ^. _UserSanitizedPackResponse .. user_ ^. _UserSanitizedResponse .. nick_) []
      }
    ) st.users) pNum
  where
  pNum = renderPageNumbers st.usersPageInfo st.currentPage
