module LN.State.Internal.Types (
  InternalState
) where



import Control.Monad.Aff.AVar       (AVar())
import Data.Map                     as M
import Data.Maybe                   (Maybe(..))
import Data.Tuple                   (Tuple)
import Prelude                      (Unit)

import LN.Component.CreateThread    (Comp_CreateThread_State)
-- import LN.Input.Types               (Input)
-- import LN.Router.Types              (Routes(..))
import LN.State.Leuron              (LeuronRequestState, defaultLeuronRequestState)
import LN.State.Loading             (LoadingMap, defaultLoadingMap)
import LN.State.Resource            (ResourceRequestState, defaultResourceRequestState)
import LN.State.PageInfo            ( PageInfo
                                    , defaultPageInfo
                                    , defaultPageInfo_Organizations
                                    , defaultPageInfo_Users
                                    , defaultPageInfo_Threads
                                    , defaultPageInfo_ThreadPosts
                                    , defaultPageInfo_Resources
                                    , defaultPageInfo_Leurons)
import LN.T



type InternalState routes =
  { currentPage           :: routes
  , me                    :: Maybe UserPackResponse
  , meId                  :: Int
  , errors                :: Array (Tuple String String)
  , users                 :: M.Map Int UserSanitizedPackResponse
  , usersMap              :: M.Map Int UserSanitizedPackResponse
  , organizations         :: M.Map Int OrganizationPackResponse
  , teams                 :: M.Map Int TeamPackResponse
  , forums                :: M.Map Int ForumPackResponse
  , boards                :: M.Map Int BoardPackResponse
  , threads               :: M.Map Int ThreadPackResponse
  , threadPosts           :: M.Map Int ThreadPostPackResponse
  , resources             :: M.Map Int ResourcePackResponse
  , leurons               :: M.Map Int LeuronPackResponse
--  , workouts            :: M.Map Int WorkoutPackResponse
--  , pms                 :: Maybe (Array PmResponse)
  , currentOrganization   :: Maybe OrganizationPackResponse
  , currentUser           :: Maybe UserSanitizedPackResponse
--  , currentTeam         :: Maybe TeamResponse
  , currentForum          :: Maybe ForumPackResponse
  , currentBoard          :: Maybe BoardPackResponse
  , currentThread         :: Maybe ThreadPackResponse
  , currentThreadPost     :: Maybe ThreadPostRequest -- TODO FIXME: rename to something more appropriate
  , currentResource       :: Maybe ResourcePackResponse
  , currentResourceRequest :: Maybe ResourceRequest
  , currentResourceRequestSt :: Maybe ResourceRequestState
  , currentLeuron         :: Maybe LeuronPackResponse
  , currentLeuronRequest  :: Maybe LeuronRequest
  , currentLeuronRequestSt :: Maybe LeuronRequestState
--  , currentWorkout      :: Maybe WorkoutResponse
  , currentPageInfo       :: PageInfo
  , organizationsPageInfo :: PageInfo
  , usersPageInfo         :: PageInfo
  , threadsPageInfo       :: PageInfo
  , threadPostsPageInfo   :: PageInfo
  , resourcesPageInfo     :: PageInfo
  , leuronsPageInfo       :: PageInfo
  , compCreateThread      :: Maybe Comp_CreateThread_State
--  , driverCh              :: AVar (Input Unit)
  , loading               :: LoadingMap
  }



{-
initialInternalState :: AVar (Input Unit) -> InternalState
initialInternalState ch =
  { currentPage:           Home
  , me:                    Nothing
  , meId:                  0
  , errors:                []
  , users:                 M.empty
  , usersMap:              M.empty
  , organizations:         M.empty
  , teams:                 M.empty
  , forums:                M.empty
  , boards:                M.empty
  , threads:               M.empty
  , threadPosts:           M.empty
  , resources:             M.empty
  , leurons:               M.empty
--  , workouts:            M.empty
  , currentOrganization:   Nothing
  , currentUser:           Nothing
--  , currentTeam:         Nothing
  , currentForum:          Nothing
  , currentBoard:          Nothing
  , currentThread:         Nothing
  , currentThreadPost:     Nothing
  , currentResource:       Nothing
  , currentResourceRequest: Nothing
  , currentResourceRequestSt: Nothing
  , currentLeuron:         Nothing
  , currentLeuronRequest:  Nothing
  , currentLeuronRequestSt: Nothing
--  , currentWorkout:      Nothing
  , currentPageInfo:       defaultPageInfo
  , organizationsPageInfo: defaultPageInfo_Organizations
  , usersPageInfo:         defaultPageInfo_Users
  -- need dsc by modifiedAt !!!!!!!! TODO FIXME
  , threadsPageInfo:       defaultPageInfo_Threads
  , threadPostsPageInfo:   defaultPageInfo_ThreadPosts
  , resourcesPageInfo:     defaultPageInfo_Resources
  , leuronsPageInfo:       defaultPageInfo_Leurons
  , compCreateThread:      Nothing
--  , driverCh:              ch
  , loading:               defaultLoadingMap
  }
-}
