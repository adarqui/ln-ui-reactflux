module LN.UI.State.Internal.Types (
  InternalState (..)
) where



import qualified Data.Map                 as M
import           Data.Maybe               (Maybe)

import           LN.T
import           LN.UI.State.Board        (BoardRequestState)
import           LN.UI.State.Forum        (ForumRequestState)
import           LN.UI.State.Leuron       (LeuronRequestState)
import           LN.UI.State.Loading      (LoadingMap)
import           LN.UI.State.Organization (OrganizationRequestState)
import           LN.UI.State.PageInfo     (PageInfo)
import           LN.UI.State.Resource     (ResourceRequestState)
import           LN.UI.State.Team         (TeamRequestState)
import           LN.UI.State.TeamMember   (TeamMemberRequestState)
import           LN.UI.State.Thread       (ThreadRequestState)
import           LN.UI.State.ThreadPost   (ThreadPostRequestState)
import           LN.UI.Types              (Array, Tuple)



data InternalState routes {- TODO FIXME: driver_ch-} = InternalState
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
  , recentThreadPosts            :: M.Map Int ThreadPostPackResponse
  , motwThreadPosts              :: M.Map Int ThreadPostPackResponse
  , resources                    :: M.Map Int ResourcePackResponse
  , leurons                      :: M.Map Int LeuronPackResponse
  , pmsIn                        :: M.Map Int PmInPackResponse
  , pmsOut                       :: M.Map Int PmOutPackResponse
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
  , currentPm                    :: Maybe PmResponse
  , currentPmRequest             :: Maybe PmRequest
  , currentPmIn                  :: Maybe PmInResponse
  , currentPmInRequest           :: Maybe PmInRequest
  , currentPmOut                 :: Maybe PmOutResponse
  , currentPmOutRequest          :: Maybe PmOutRequest
  , currentPageInfo              :: PageInfo
  , organizationsPageInfo        :: PageInfo
  , usersPageInfo                :: PageInfo
  , threadsPageInfo              :: PageInfo
  , threadPostsPageInfo          :: PageInfo
  , resourcesPageInfo            :: PageInfo
  , leuronsPageInfo              :: PageInfo
  , loading                      :: LoadingMap
  }
