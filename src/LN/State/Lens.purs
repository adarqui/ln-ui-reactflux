module LN.State.Lens where



import Data.Map                  as M
import Data.Maybe                (Maybe(..), maybe)
import Optic.Core                (LensP, lens, (^.), (..))
import Prelude                   (($))

import LN.Component.CreateThread (Comp_CreateThread_State)
import LN.Router.Types           (Routes)
import LN.State.PageInfo         (PageInfo)
import LN.State.Types            (State)
import LN.T



stMe :: LensP State (Maybe UserPackResponse)
stMe =
  lens
    _.me
    (_ { me = _ })



stErrors :: LensP State (Maybe (Array String))
stErrors = lens _.errors _ { errors = _ }



stOrganizations :: LensP State (Array OrganizationResponse)
stOrganizations =
  lens
    _.organizations
    (_ { organizations = _ })



stUsers :: LensP State (Array UserSanitizedPackResponse)
stUsers =
  lens
    _.users
    (_ { users = _ })



stUsersMap :: LensP State (M.Map Int UserSanitizedPackResponse)
stUsersMap =
  lens
    _.usersMap
    (_ { usersMap = _ })



usersMapLookup :: State -> Int -> Maybe UserSanitizedPackResponse
usersMapLookup st user_id =
  M.lookup user_id (st ^. stUsersMap)



usersMapLookup_ToUser :: State -> Int -> Maybe UserSanitizedResponse
usersMapLookup_ToUser st user_id =
  maybe Nothing (\user -> Just $ user ^. _UserSanitizedPackResponse .. user_) $ M.lookup user_id (st ^. stUsersMap)



usersMapLookup_ToNick :: State -> Int -> String
usersMapLookup_ToNick st user_id =
  maybe "unknown" (\user -> user ^. _UserSanitizedResponse .. nick_) (usersMapLookup_ToUser st user_id)



stTeams :: LensP State (Array TeamResponse)
stTeams =
  lens
    _.teams
    (_ { teams = _ })



stForums :: LensP State (Array ForumResponse)
stForums =
  lens
    _.forums
    (_ { forums = _ })



stBoardPacks :: LensP State (Array BoardPackResponse)
stBoardPacks =
  lens
    _.boardPacks
    (_ { boardPacks = _ })



stThreadPacks :: LensP State (Array ThreadPackResponse)
stThreadPacks =
  lens
    _.threadPacks
    (_ { threadPacks = _ })



stThreadPosts :: LensP State (Array ThreadPostPackResponse)
stThreadPosts =
  lens
    _.threadPosts
    (_ { threadPosts = _ })



stCurrentOrganization :: LensP State (Maybe OrganizationResponse)
stCurrentOrganization =
  lens
    (_.currentOrganization)
    (_ { currentOrganization = _ })



stCurrentUser :: LensP State (Maybe UserSanitizedPackResponse)
stCurrentUser =

  lens
    (_.currentUser)
    (_ { currentUser = _ })



stCurrentForum :: LensP State (Maybe ForumResponse)
stCurrentForum =
  lens
    (_.currentForum)
    (_ { currentForum = _ })



stCurrentBoard :: LensP State (Maybe BoardResponse)
stCurrentBoard =
  lens
    (_.currentBoard)
    (_ { currentBoard = _ })



stCurrentThread :: LensP State (Maybe ThreadResponse)
stCurrentThread =
  lens
    (_.currentThread)
    (_ { currentThread = _ })



stCurrentThreadPost :: LensP State (Maybe ThreadPostRequest)
stCurrentThreadPost =
  lens
    (_.currentThreadPost)
    (_ { currentThreadPost = _ })



stCurrentPage :: LensP State Routes
stCurrentPage =
  lens
    _.currentPage
    (_ { currentPage = _ })



stCurrentPageInfo :: LensP State PageInfo
stCurrentPageInfo =
  lens
    _.currentPageInfo
    (_ { currentPageInfo = _ })



stOrganizationsPageInfo :: LensP State PageInfo
stOrganizationsPageInfo =
  lens
    _.organizationsPageInfo
    (_ { organizationsPageInfo = _ })



stUsersPageInfo :: LensP State PageInfo
stUsersPageInfo =
  lens
    _.usersPageInfo
    (_ { usersPageInfo = _ })



stThreadsPageInfo :: LensP State PageInfo
stThreadsPageInfo =
  lens
    _.threadsPageInfo
    (_ { threadsPageInfo = _ })



stThreadPostsPageInfo :: LensP State PageInfo
stThreadPostsPageInfo =
  lens
    _.threadPostsPageInfo
    (_ { threadPostsPageInfo = _ })



stCompCreateThread :: LensP State (Maybe Comp_CreateThread_State)
stCompCreateThread =
  lens
    _.compCreateThread
    (_ { compCreateThread = _ })
