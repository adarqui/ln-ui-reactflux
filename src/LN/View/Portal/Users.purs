module LN.View.Portal.Users (
  renderView_Portal_Users
) where


import Halogen                      (ComponentHTML)
import Optic.Core                   ((^.), (..))
import Prelude                      (map, ($))

import LN.Input.Types               (Input)
import LN.Router.Types              (Routes(..), CRUD(..))
import LN.State.Types               (State)
import LN.State.Lens
import LN.T
import LN.View.Module.Gravatar
import LN.View.Module.PageNumbers
import LN.View.Module.EntityListing



renderView_Portal_Users :: State -> ComponentHTML Input
renderView_Portal_Users st =
  renderEntityListing "Users" (
    map (\user ->
      { nick: user ^. _UserSanitizedPackResponse .. user_ ^. _UserSanitizedResponse .. nick_
      , displayNick: user ^. _UserSanitizedPackResponse .. user_ ^. _UserSanitizedResponse .. displayNick_
      , createdAt: user ^. _UserSanitizedPackResponse .. user_ ^. _UserSanitizedResponse .. createdAt_
      , logo: gravatarUrlFromUser XLarge (user ^. _UserSanitizedPackResponse .. user_)
      , route: Users (Show $ user ^. _UserSanitizedPackResponse .. user_ ^. _UserSanitizedResponse .. nick_)
      }
    ) st.users) pNum
  where
  pNum = renderPageNumbers (st ^. stUsersPageInfo) (st ^. stCurrentPage)
