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
import LN.State.PageInfo            (PageInfo
                                    , defaultPageInfo
                                    , defaultPageInfo_Organizations
                                    , defaultPageInfo_Users
                                    , defaultPageInfo_Threads
                                    , defaultPageInfo_ThreadPosts
                                    , defaultPageInfo_Resources
                                    , defaultPageInfo_Leurons)
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
  , resources :: M.Map Int ResourcePackResponse
  , leurons :: M.Map Int LeuronPackResponse
--  , workouts :: M.Map Int WorkoutPackResponse
--  , pms :: Maybe (Array PmResponse)
  , currentOrganization :: Maybe OrganizationResponse
  , currentUser :: Maybe UserSanitizedPackResponse
--  , currentTeam :: Maybe TeamResponse
  , currentForum :: Maybe ForumResponse
  , currentBoard :: Maybe BoardResponse
  , currentThread :: Maybe ThreadResponse
  , currentThreadPost :: Maybe ThreadPostRequest
  , currentResource :: Maybe ResourceResponse
  , currentLeuron :: Maybe LeuronResponse
--  , currentWorkout :: Maybe WorkoutResponse
  , currentPageInfo :: PageInfo
  , organizationsPageInfo :: PageInfo
  , usersPageInfo :: PageInfo
  , threadsPageInfo :: PageInfo
  , threadPostsPageInfo :: PageInfo
  , resourcesPageInfo :: PageInfo
  , leuronsPageInfo :: PageInfo
  , compCreateThread :: Maybe Comp_CreateThread_State
  , driverCh :: AVar (Input Unit)
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
  , resources: M.empty
  , leurons: M.empty
--  , workouts: M.empty
  , currentOrganization: Nothing
  , currentUser: Nothing
--  , currentTeam: Nothing
  , currentForum: Nothing
  , currentBoard: Nothing
  , currentThread: Nothing
  , currentThreadPost: Nothing
  , currentResource: Nothing
  , currentLeuron: Nothing
--  , currentWorkout: Nothing
  , currentPageInfo: defaultPageInfo
  , organizationsPageInfo: defaultPageInfo_Organizations
  , usersPageInfo: defaultPageInfo_Users
  -- need dsc by modifiedAt !!!!!!!! TODO FIXME
  , threadsPageInfo: defaultPageInfo_Threads
  , threadPostsPageInfo: defaultPageInfo_ThreadPosts
  , resourcesPageInfo: defaultPageInfo_Resources
  , leuronsPageInfo: defaultPageInfo_Leurons
  , compCreateThread: Nothing
  , driverCh: ch
  }
