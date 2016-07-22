module LN.State.Types (
  State,
  DriverCh,
  initialState
) where



import LN.Router.Class.Route       (Routes)
import LN.State.Internal.Types      (InternalState)
import Control.Monad.Aff.AVar       (AVar())
import Data.Map                     as M
import Data.Maybe                   (Maybe(..))
import Data.Tuple                   (Tuple)
import Prelude                      (Unit)

-- import LN.Router.Types              (Routes(..))
import LN.Component.CreateThread    (Comp_CreateThread_State)
import LN.Input.Types               (Input)
import LN.Router.Class.Route       (Routes(..))
import LN.State.ArrayString         (defaultArrayStringState)
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



-- type State    = InternalState Routes DriverCh -- TODO FIXME CYCLIC
type State = InternalState Routes



type DriverCh = AVar (Input Unit)



initialState :: AVar (Input Unit) -> State
initialState ch =
  { currentPage:                  Home
  , me:                           Nothing
  , meId:                         0
  , errors:                       []
  , users:                        M.empty
  , usersMap:                     M.empty
  , organizations:                M.empty
  , teams:                        M.empty
  , teamMembers:                  M.empty
  , forums:                       M.empty
  , boards:                       M.empty
  , threads:                      M.empty
  , threadPosts:                  M.empty
  , recentThreadPosts:            M.empty
  , motwThreadPosts:              M.empty
  , resources:                    M.empty
  , leurons:                      M.empty
--  , workouts:                   M.empty
  , pmsIn:                        M.empty
  , pmsOut:                       M.empty
  , currentOrganization:          Nothing
  , currentOrganizationRequest:   Nothing
  , currentOrganizationRequestSt: Nothing
  , currentUser:                  Nothing
  , currentTeam:                  Nothing
  , currentTeamRequest:           Nothing
  , currentTeamRequestSt:         Nothing
  , currentTeamMember:            Nothing
  , currentTeamMemberRequest:     Nothing
  , currentTeamMemberRequestSt:   Nothing
  , currentForum:                 Nothing
  , currentForumRequest:          Nothing
  , currentForumRequestSt:        Nothing
  , currentBoard:                 Nothing
  , currentBoardRequest:          Nothing
  , currentBoardRequestSt:        Nothing
  , currentThread:                Nothing
  , currentThreadRequest:         Nothing
  , currentThreadRequestSt:       Nothing
  , currentThreadPost:            Nothing
  , currentThreadPostRequest:     Nothing
  , currentThreadPostRequestSt:   Nothing
  , currentResource:              Nothing
  , currentResourceRequest:       Nothing
  , currentResourceRequestSt:     Nothing
  , currentLeuron:                Nothing
  , currentLeuronRequest:         Nothing
  , currentLeuronRequestSt:       Nothing
--  , currentWorkout:             Nothing
  , currentPm:                    Nothing
  , currentPmRequest:             Nothing
  , currentPmIn:                  Nothing
  , currentPmInRequest:           Nothing
  , currentPmOut:                 Nothing
  , currentPmOutRequest:          Nothing
  , currentPageInfo:              defaultPageInfo
  , organizationsPageInfo:        defaultPageInfo_Organizations
  , usersPageInfo:                defaultPageInfo_Users
  -- need dsc by modifiedAt !!!!!!!! TODO FIXME
  , threadsPageInfo:              defaultPageInfo_Threads
  , threadPostsPageInfo:          defaultPageInfo_ThreadPosts
  , resourcesPageInfo:            defaultPageInfo_Resources
  , leuronsPageInfo:              defaultPageInfo_Leurons
  , arrayStringSt:                defaultArrayStringState
--  , driverCh:                     ch
  , loading:                      defaultLoadingMap
  }
