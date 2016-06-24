module LN.State.Internal.Types (
  InternalState
) where



-- import Control.Monad.Aff.AVar       (AVar())
import Data.Map                     as M
import Data.Maybe                   (Maybe)
import Data.Tuple                   (Tuple)

-- import LN.Input.Types               (Input)
-- import LN.Router.Types              (Routes(..))
import LN.State.ArrayString         (ArrayStringState)
import LN.State.Board               (BoardRequestState)
import LN.State.Forum               (ForumRequestState)
import LN.State.Leuron              (LeuronRequestState)
import LN.State.Loading             (LoadingMap)
import LN.State.Organization        (OrganizationRequestState)
import LN.State.Resource            (ResourceRequestState)
import LN.State.PageInfo            (PageInfo)
import LN.State.Thread              (ThreadRequestState)
import LN.State.ThreadPost          (ThreadPostRequestState)
import LN.State.Team                (TeamRequestState)
import LN.State.TeamMember          (TeamMemberRequestState)
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
  , teamMembers                  :: M.Map Int TeamMemberPackResponse
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
  , currentOrganizationRequestSt :: Maybe OrganizationRequestState
  , currentTeam                  :: Maybe TeamPackResponse
  , currentTeamRequest           :: Maybe TeamRequest
  , currentTeamRequestSt         :: Maybe TeamRequestState
  , currentTeamMember            :: Maybe TeamMemberPackResponse
  , currentTeamMemberRequest     :: Maybe TeamMemberRequest
  , currentTeamMemberRequestSt   :: Maybe TeamMemberRequestState
  , currentUser                  :: Maybe UserSanitizedPackResponse
  , currentForum                 :: Maybe ForumPackResponse
  , currentForumRequest          :: Maybe ForumRequest
  , currentForumRequestSt        :: Maybe ForumRequestState
  , currentBoard                 :: Maybe BoardPackResponse
  , currentBoardRequest          :: Maybe BoardRequest
  , currentBoardRequestSt        :: Maybe BoardRequestState
  , currentThread                :: Maybe ThreadPackResponse
  , currentThreadRequest         :: Maybe ThreadRequest
  , currentThreadRequestSt       :: Maybe ThreadRequestState
  , currentThreadPost            :: Maybe ThreadPostPackResponse
  , currentThreadPostRequest     :: Maybe ThreadPostRequest
  , currentThreadPostRequestSt   :: Maybe ThreadPostRequestState
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
  , arrayStringSt                :: ArrayStringState
--  , driverCh                     :: driver_ch
--  , driverCh                   :: AVar (Input Unit)
  , loading                      :: LoadingMap
  }
