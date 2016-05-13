module LN.State.Types (
  State,
  initialState
) where



import Control.Monad.Aff.AVar       (AVar())
import Data.Map                     as M
import Data.Maybe                   (Maybe(..))
import Prelude                      (Unit)

import LN.Router.Types              (Routes(..))
import LN.Component.CreateThread    (Comp_CreateThread_State)
import LN.Input.Types               (Input)
import LN.State.PageInfo            (PageInfo, defaultPageInfo, defaultPageInfo_Organizations, defaultPageInfo_Users
                                    , defaultPageInfo_Threads, defaultPageInfo_ThreadPosts)
import LN.T



type State =
  { currentPage :: Routes
  , me :: Maybe UserPackResponse
  , meId :: Int
  , errors :: Maybe (Array String)
  , users :: Array UserSanitizedPackResponse
  , usersMap :: M.Map Int UserSanitizedPackResponse
  , organizations :: Array OrganizationResponse
  , teams :: Array TeamResponse
  , forums :: Array ForumResponse
  , boardPacks :: Array BoardPackResponse
  , threadPacks :: Array ThreadPackResponse
  , threadPosts :: M.Map Int ThreadPostPackResponse
--  , threadPostLikes :: Maybe (Array ThreadPostLikeResponse
--  , pms :: Maybe (Array PmResponse)
--  , resources :: Maybe (Array ResourceResponse)
--  , leurons :: Maybe (Array LeuronResponse)
  , currentOrganization :: Maybe OrganizationResponse
  , currentUser :: Maybe UserSanitizedPackResponse
--  , currentTeam :: Maybe TeamResponse
--  , me :: Maybe UserResponse
  , currentForum :: Maybe ForumResponse
  , currentBoard :: Maybe BoardResponse
  , currentThread :: Maybe ThreadResponse
  , currentThreadPost :: Maybe ThreadPostRequest
--  , currentResource :: Maybe ResourceResponse
--  , currentLeuron :: Maybe LeuronResponse
  , currentPageInfo :: PageInfo
  , organizationsPageInfo :: PageInfo
  , usersPageInfo :: PageInfo
  , threadsPageInfo :: PageInfo
  , threadPostsPageInfo :: PageInfo
  , compCreateThread :: Maybe Comp_CreateThread_State
  , driverCh :: AVar (Input Unit)
--  , queryChan :: Maybe AVar (Input Unit)
  }



initialState :: AVar (Input Unit) -> State
initialState ch =
  { currentPage: Home
  , me: Nothing
  , meId: 0
  , errors: Nothing
  , users: []
  , usersMap: M.empty
  , organizations: []
  , teams: []
  , forums: []
  , boardPacks: []
  , threadPacks: []
  , threadPosts: M.empty
  , currentOrganization: Nothing
  , currentUser: Nothing
--  , currentTeam: Nothing
  , currentForum: Nothing
  , currentBoard: Nothing
  , currentThread: Nothing
  , currentThreadPost: Nothing
--  , currentResource: Nothing
--  , currentLeuron: Nothing
  , currentPageInfo: defaultPageInfo
  , organizationsPageInfo: defaultPageInfo_Organizations
  , usersPageInfo: defaultPageInfo_Users
  -- need dsc by modifiedAt !!!!!!!! TODO FIXME
  , threadsPageInfo: defaultPageInfo_Threads
  , threadPostsPageInfo: defaultPageInfo_ThreadPosts
  , compCreateThread: Nothing
  , driverCh: ch
  }
