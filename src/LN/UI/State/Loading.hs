module LN.UI.State.Loading (
  LoadingMap,
  defaultLoadingMap,
  getLoading,
  setLoading,
  clearLoading,
  l_me,
  l_users,
  l_organizations,
  l_teams,
  l_teamMembers,
  l_forums,
  l_boards,
  l_threads,
  l_threadPosts,
  l_resources,
  l_leurons,
  l_currentOrganization,
  l_currentOrganizationRequest,
  l_currentTeam,
  l_currentTeamRequest,
  l_currentTeamMember,
  l_currentTeamMemberRequest,
  l_currentUser,
  l_currentForum,
  l_currentForumRequest,
  l_currentBoard,
  l_currentBoardRequest,
  l_currentThread,
  l_currentThreadRequest,
  l_currentThreadPost,
  l_currentThreadPostRequest,
  l_currentResource,
  l_currentResourceRequest,
  l_currentLeuron,
  l_currentLeuronRequest
) where



import           Data.Map     as M (Map, fromList, update, lookup)
import           Data.Maybe   (Maybe (..), fromJust)
import           Prelude      (Int, Bool(..), const, map, ($))

import           LN.UI.Types  (List, Tuple)



type LoadingMap = M.Map Int Bool



defaultLoadingMap :: LoadingMap
defaultLoadingMap = M.fromList $ map (\s -> (s, False)) loadingKeys



getLoading :: Int -> LoadingMap -> Bool
getLoading key lm = fromJust $ M.lookup key lm



setLoading :: Int -> LoadingMap -> LoadingMap
setLoading key lm = M.update (const $ Just True) key lm



clearLoading :: Int -> LoadingMap -> LoadingMap
clearLoading key lm = M.update (const $ Just False) key lm




loadingKeys :: List Int
loadingKeys =
  [ l_me
  , l_users
  , l_organizations
  , l_teams
  , l_teamMembers
  , l_forums
  , l_boards
  , l_threads
  , l_threadPosts
  , l_resources
  , l_leurons
  , l_currentOrganization
  , l_currentOrganizationRequest
  , l_currentTeam
  , l_currentTeamRequest
  , l_currentTeamMember
  , l_currentTeamMemberRequest
  , l_currentUser
  , l_currentForum
  , l_currentForumRequest
  , l_currentBoard
  , l_currentBoardRequest
  , l_currentThread
  , l_currentThreadRequest
  , l_currentThreadPost
  , l_currentThreadPostRequest
  , l_currentResource
  , l_currentResourceRequest
  , l_currentLeuron
  , l_currentLeuronRequest
  ]



-- TODO FIXME: This should be an Enum

l_me                            :: Int
l_me                            = 0

l_users                         :: Int
l_users                         = 1

l_organizations                 :: Int
l_organizations                 = 2

l_teams                         :: Int
l_teams                         = 3

l_forums                        :: Int
l_forums                        = 4

l_boards                        :: Int
l_boards                        = 5

l_threads                       :: Int
l_threads                       = 6

l_threadPosts                   :: Int
l_threadPosts                   = 7

l_resources                     :: Int
l_resources                     = 8

l_leurons                       :: Int
l_leurons                       = 9

l_currentOrganization           :: Int
l_currentOrganization           = 10

l_currentOrganizationRequest    :: Int
l_currentOrganizationRequest    = 11

l_currentTeam                   :: Int
l_currentTeam                   = 12

l_currentTeamRequest            :: Int
l_currentTeamRequest            = 13

l_currentUser                   :: Int
l_currentUser                   = 14

l_currentForum                  :: Int
l_currentForum                  = 15

l_currentForumRequest           :: Int
l_currentForumRequest           = 16

l_currentBoard                  :: Int
l_currentBoard                  = 17

l_currentBoardRequest           :: Int
l_currentBoardRequest           = 18

l_currentThread                 :: Int
l_currentThread                 = 19

l_currentThreadRequest          :: Int
l_currentThreadRequest          = 20

l_currentThreadPost             :: Int
l_currentThreadPost             = 21

l_currentThreadPostRequest      :: Int
l_currentThreadPostRequest      = 22

l_currentResource               :: Int
l_currentResource               = 23

l_currentResourceRequest        :: Int
l_currentResourceRequest        = 24

l_currentLeuron                 :: Int
l_currentLeuron                 = 25

l_currentLeuronRequest          :: Int
l_currentLeuronRequest          = 26

l_teamMembers                   :: Int
l_teamMembers                   = 27

l_currentTeamMember             :: Int
l_currentTeamMember             = 28

l_currentTeamMemberRequest      :: Int
l_currentTeamMemberRequest      = 29
