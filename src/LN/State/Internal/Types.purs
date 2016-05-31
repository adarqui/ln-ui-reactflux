module LN.State.Internal.Types (
  InternalState
) where



-- import Control.Monad.Aff.AVar       (AVar())
import Data.Map                     as M
import Data.Maybe                   (Maybe)
import Data.Tuple                   (Tuple)

import LN.Component.CreateThread    (Comp_CreateThread_State)
-- import LN.Input.Types               (Input)
-- import LN.Router.Types              (Routes(..))
import LN.State.Forum               (ForumRequestState)
import LN.State.Leuron              (LeuronRequestState)
import LN.State.Loading             (LoadingMap)
import LN.State.Resource            (ResourceRequestState)
import LN.State.PageInfo            (PageInfo)
import LN.T



type InternalState routes {- TODO FIXME: driver_ch-} =
  { currentPage                  :: routes
  , me                           :: Maybe UserPackResponse
  , meId                         :: Int
  , errors                       :: Array (Tuple String String)
  , users                        :: M.Map Int UserSanitizedPackResponse
  , usersMap                     :: M.Map Int UserSanitizedPackResponse
  , organizations                :: M.Map Int OrganizationPackResponse
  , teams                        :: M.Map Int TeamPackResponse
  , forums                       :: M.Map Int ForumPackResponse
  , boards                       :: M.Map Int BoardPackResponse
  , threads                      :: M.Map Int ThreadPackResponse
  , threadPosts                  :: M.Map Int ThreadPostPackResponse
  , resources                    :: M.Map Int ResourcePackResponse
  , leurons                      :: M.Map Int LeuronPackResponse
--  , workouts                   :: M.Map Int WorkoutPackResponse
--  , pms                        :: Maybe (Array PmResponse)
  , currentOrganization          :: Maybe OrganizationPackResponse
  , currentOrganizationRequest   :: Maybe OrganizationRequest
  , currentOrganizationRequestSt :: Maybe Int
  , currentTeam                  :: Maybe TeamResponse
  , currentTeamRequest           :: Maybe TeamRequest
  , currentTeamRequestSt         :: Maybe Int
  , currentUser                  :: Maybe UserSanitizedPackResponse
  , currentForum                 :: Maybe ForumPackResponse
  , currentForumRequest          :: Maybe ForumRequest
  , currentForumRequestSt        :: Maybe ForumRequestState
  , currentBoard                 :: Maybe BoardPackResponse
  , currentBoardRequest          :: Maybe BoardRequest
  , currentBoardRequestSt        :: Maybe Int
  , currentThread                :: Maybe ThreadPackResponse
  , currentThreadRequest         :: Maybe ThreadRequest
  , currentThreadRequestSt       :: Maybe Int
  , currentThreadPost            :: Maybe ThreadPostRequest -- TODO FIXME: rename to something more appropriate
  , currentThreadPostRequest     :: Maybe ThreadPostRequest
  , currentThreadPostRequestSt   :: Maybe Int
  , currentResource              :: Maybe ResourcePackResponse
  , currentResourceRequest       :: Maybe ResourceRequest
  , currentResourceRequestSt     :: Maybe ResourceRequestState
  , currentLeuron                :: Maybe LeuronPackResponse
  , currentLeuronRequest         :: Maybe LeuronRequest
  , currentLeuronRequestSt       :: Maybe LeuronRequestState
--  , currentWorkout             :: Maybe WorkoutResponse
  , currentPageInfo              :: PageInfo
  , organizationsPageInfo        :: PageInfo
  , usersPageInfo                :: PageInfo
  , threadsPageInfo              :: PageInfo
  , threadPostsPageInfo          :: PageInfo
  , resourcesPageInfo            :: PageInfo
  , leuronsPageInfo              :: PageInfo
  , compCreateThread             :: Maybe Comp_CreateThread_State
--  , driverCh                     :: driver_ch
--  , driverCh                   :: AVar (Input Unit)
  , loading                      :: LoadingMap
  }
