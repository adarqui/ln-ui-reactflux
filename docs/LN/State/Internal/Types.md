## Module LN.State.Internal.Types

#### `InternalState`

``` purescript
type InternalState routes = { currentPage :: routes, me :: Maybe UserPackResponse, meId :: Int, errors :: Array (Tuple String String), users :: Map Int UserSanitizedPackResponse, usersMap :: Map Int UserSanitizedPackResponse, organizations :: Map Int OrganizationPackResponse, teams :: Map Int TeamPackResponse, forums :: Map Int ForumPackResponse, boards :: Map Int BoardPackResponse, threads :: Map Int ThreadPackResponse, threadPosts :: Map Int ThreadPostPackResponse, resources :: Map Int ResourcePackResponse, leurons :: Map Int LeuronPackResponse, currentOrganization :: Maybe OrganizationPackResponse, currentUser :: Maybe UserSanitizedPackResponse, currentForum :: Maybe ForumPackResponse, currentBoard :: Maybe BoardPackResponse, currentThread :: Maybe ThreadPackResponse, currentThreadPost :: Maybe ThreadPostRequest, currentResource :: Maybe ResourcePackResponse, currentResourceRequest :: Maybe ResourceRequest, currentResourceRequestSt :: Maybe ResourceRequestState, currentLeuron :: Maybe LeuronPackResponse, currentLeuronRequest :: Maybe LeuronRequest, currentLeuronRequestSt :: Maybe LeuronRequestState, currentPageInfo :: PageInfo, organizationsPageInfo :: PageInfo, usersPageInfo :: PageInfo, threadsPageInfo :: PageInfo, threadPostsPageInfo :: PageInfo, resourcesPageInfo :: PageInfo, leuronsPageInfo :: PageInfo, compCreateThread :: Maybe Comp_CreateThread_State, loading :: LoadingMap }
```


