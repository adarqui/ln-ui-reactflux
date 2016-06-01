module LN.State.Lens (
  stMe,
  stErrors,
  stOrganizations,
  stUsers,
  stUsersMap,
  stTeams,
  stForums,
  stBoards,
  stThreads,
  stThreadPosts,
  stCurrentOrganization,
  stCurrentUser,
  stCurrentForum,
  stCurrentBoard,
  stCurrentThread,
  stCurrentThreadPost,
  stCurrentPage,
  stCurrentPageInfo,
  stOrganizationsPageInfo,
  stUsersPageInfo,
  stThreadsPageInfo,
  stThreadPostsPageInfo,
  stCompCreateThread
) where



import Data.Map                  as M
import Data.Maybe                (Maybe)
import Data.Tuple                (Tuple)
import Optic.Core                (LensP, lens)

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



stErrors :: LensP State (Array (Tuple String String))
stErrors = lens _.errors _ { errors = _ }



stOrganizations :: LensP State (M.Map Int OrganizationPackResponse)
stOrganizations =
  lens
    _.organizations
    (_ { organizations = _ })



stUsers :: LensP State (M.Map Int UserSanitizedPackResponse)
stUsers =
  lens
    _.users
    (_ { users = _ })



stUsersMap :: LensP State (M.Map Int UserSanitizedPackResponse)
stUsersMap =
  lens
    _.usersMap
    (_ { usersMap = _ })



stTeams :: LensP State (M.Map Int TeamPackResponse)
stTeams =
  lens
    _.teams
    (_ { teams = _ })



stForums :: LensP State (M.Map Int ForumPackResponse)
stForums =
  lens
    _.forums
    (_ { forums = _ })



stBoards :: LensP State (M.Map Int BoardPackResponse)
stBoards =
  lens
    _.boards
    (_ { boards = _ })



stThreads :: LensP State (M.Map Int ThreadPackResponse)
stThreads =
  lens
    _.threads
    (_ { threads = _ })



stThreadPosts :: LensP State (M.Map Int ThreadPostPackResponse)
stThreadPosts =
  lens
    _.threadPosts
    (_ { threadPosts = _ })



stCurrentOrganization :: LensP State (Maybe OrganizationPackResponse)
stCurrentOrganization =
  lens
    (_.currentOrganization)
    (_ { currentOrganization = _ })



stCurrentUser :: LensP State (Maybe UserSanitizedPackResponse)
stCurrentUser =

  lens
    (_.currentUser)
    (_ { currentUser = _ })



stCurrentForum :: LensP State (Maybe ForumPackResponse)
stCurrentForum =
  lens
    (_.currentForum)
    (_ { currentForum = _ })



stCurrentBoard :: LensP State (Maybe BoardPackResponse)
stCurrentBoard =
  lens
    (_.currentBoard)
    (_ { currentBoard = _ })



stCurrentThread :: LensP State (Maybe ThreadPackResponse)
stCurrentThread =
  lens
    (_.currentThread)
    (_ { currentThread = _ })



stCurrentThreadPost :: LensP State (Maybe ThreadPostPackResponse)
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
