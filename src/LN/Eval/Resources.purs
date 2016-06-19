module LN.Eval.Resources (
  eval_GetResources,
  eval_GetResourceId,
  eval_GetResourcesLeurons,
  eval_GetResourcesSiftLeurons,
  eval_GetResourceLeuronLinear,
  eval_GetResourceLeuronRandom,
  eval_Resource
) where



import Data.Array                    (head, deleteAt, modifyAt, nub)
import Data.Either                   (Either(..))
import Data.Functor                  (($>))
import Data.Map                      as M
import Data.Maybe                    (Maybe(..), maybe)
import Halogen                       (gets, modify)
import Optic.Core                    ((^.), (..), (.~))
import Prelude                       (class Eq, id, const, bind, pure, map, ($), (<>))

import LN.Api                        ( rd
                                     , getResourcesCount', getResourcePacks, getResourcePack'
                                     , getLeuronPacks_ByResourceId
                                     , postResource', putResource')
import LN.Component.Types            (EvalEff)
import LN.Helpers.Map                (idmapFrom)
import LN.Input.Resource             (InputResource(..), Resource_Mod(..))
import LN.Input.Types                (Input(..))
import LN.Internal.Resource          (resourceTypeToTyResourceType)
import LN.Router.Types               (Routes(..), CRUD(..))
import LN.Router.Class.Params        (emptyParams)
import LN.State.Loading              (l_currentLeuron, l_currentResource, l_resources)
import LN.State.Loading.Helpers      (getLoading, setLoading, clearLoading)
import LN.State.Resource             (ResourceRequestState, defaultResourceRequestState)
import LN.State.PageInfo             (runPageInfo)
import LN.T                          ( Param(..), SortOrderBy(..)
                                     , ResourcePackResponses(..), ResourcePackResponse(..)
                                     , _ResourcePackResponse, resource_
                                     , _ResourceResponse
                                     , ResourceRequest(..), ResourceResponse(..)
                                     , _ResourceRequest
                                     , displayName_, description_, source_, visibility_
                                     , LeuronPackResponses(..))



eval_GetResources :: EvalEff
eval_GetResources eval (GetResources next) = do

  modify (_{ resources = (M.empty :: M.Map Int ResourcePackResponse) })

  page_info <- gets _.resourcesPageInfo

  e_count <- rd $ getResourcesCount'
  case e_count of
    Left err     -> eval (AddErrorApi "eval_GetResources::getResourcesCount'" err next)
    Right counts -> do

      let new_page_info = runPageInfo counts page_info

      modify (_{ resourcesPageInfo = new_page_info.pageInfo })
      modify $ setLoading l_resources

      e_resource_packs <- rd $ getResourcePacks new_page_info.params

      modify $ clearLoading l_resources

      case e_resource_packs of
           Left err                                     -> eval (AddErrorApi "eval_GetResources::getResourcePacks" err next)
           Right (ResourcePackResponses resource_packs) -> do

             let
              users         = map (\(ResourcePackResponse pack) -> pack.user) resource_packs.resourcePackResponses
              resources_map = idmapFrom (\(ResourcePackResponse p) -> p.resourceId) resource_packs.resourcePackResponses

             eval (GetUsers_MergeMap_ByUser users next)

             modify (_{ resources = resources_map })
             pure next



eval_GetResourceId :: EvalEff
eval_GetResourceId eval (GetResourceId resource_id next) = do

  modify (_{ currentResource = Nothing })
  modify $ setLoading l_currentResource

  e_pack <- rd $ getResourcePack' resource_id

  modify $ clearLoading l_currentResource

  case e_pack of
    Left err   -> pure next
    Right pack -> do
      modify (_{ currentResource = Just pack })
      pure next




eval_GetResourcesLeurons :: EvalEff
eval_GetResourcesLeurons eval (GetResourcesLeurons resource_sid next) = pure next



eval_GetResourcesSiftLeurons :: EvalEff
eval_GetResourcesSiftLeurons eval (GetResourcesSiftLeurons resource_sid next) = pure next



eval_GetResourceLeuronLinear :: EvalEff
eval_GetResourceLeuronLinear eval (GetResourceLeuronLinear resource_id offset next) = do

  modify (_{ currentLeuron = Nothing })
  modify $ setLoading l_currentLeuron

  e_packs <- rd $ getLeuronPacks_ByResourceId [Limit 1, Offset offset, SortOrder SortOrderBy_Asc] resource_id

  modify $ clearLoading l_currentLeuron

  case e_packs of
    Left err                          -> eval (AddErrorApi "eval_GetResourceLeuronLinear::getLeuronPacks_ByResourceId" err next)
    Right (LeuronPackResponses packs) -> do
      case head packs.leuronPackResponses of
        Nothing   -> pure next
        Just pack -> modify (_{ currentLeuron = Just pack }) $> next



eval_GetResourceLeuronRandom :: EvalEff
eval_GetResourceLeuronRandom eval (GetResourceLeuronRandom resource_id next) = do

  modify (_{ currentLeuron = Nothing })
  modify $ setLoading l_currentLeuron

  e_packs <- rd $ getLeuronPacks_ByResourceId [Limit 1, SortOrder SortOrderBy_Rnd] resource_id

  modify $ clearLoading l_currentLeuron

  case e_packs of
    Left err                          -> eval (AddErrorApi "eval_GetResourceLeuronRandom::getLeuronPacks_ByResourceId" err next)
    Right (LeuronPackResponses packs) -> do
      case head packs.leuronPackResponses of
        Nothing   -> pure next
        Just pack -> modify (_{ currentLeuron = Just pack }) $> next



-- | Component
--
eval_Resource :: EvalEff
eval_Resource eval (CompResource sub next) = do
  case sub of
   InputResource_Mod q -> do
     case q of
       SetDisplayName name   -> mod $ set (\req -> _ResourceRequest .. displayName_ .~ name $ req)

       SetDescription desc   -> mod $ set (\req -> _ResourceRequest .. description_ .~ desc $ req)

       SetSource source      -> mod $ set (\req -> _ResourceRequest .. source_ .~ source $ req)

       AddAuthor author'     -> mod $ set (\(ResourceRequest req) -> ResourceRequest req{ author = append req.author author' })

       DeleteAuthor idx      -> mod $ set (\(ResourceRequest req) -> ResourceRequest req{ author = maybe req.author (deleteAt idx) req.author })

       EditAuthor idx author -> mod $ set (\(ResourceRequest req) -> ResourceRequest req { author = maybe req.author (modifyAt idx (const author)) req.author })

       AddCategory cat       -> mod $ set (\(ResourceRequest req) -> ResourceRequest req{ categories = req.categories <> [cat] })

       DeleteCategory idx    -> mod $ set (\(ResourceRequest req) -> ResourceRequest req{ categories = maybe req.categories id $ deleteAt idx req.categories })

       EditCategory idx cat  -> mod $ set (\(ResourceRequest req) -> ResourceRequest req { categories = maybe req.categories id $ modifyAt idx (const cat) req.categories })

       SetVisibility viz     -> mod $ set (\req -> _ResourceRequest ..  visibility_ .~ viz $ req)

       AddUrl url            -> mod $ set (\(ResourceRequest req) -> ResourceRequest req{ urls = append req.urls url })

       DeleteUrl idx         -> mod $ set (\(ResourceRequest req) -> ResourceRequest req{ author = maybe req.urls (deleteAt idx) req.urls })

       EditUrl idx url       -> mod $ set (\(ResourceRequest req) -> ResourceRequest req{ urls = maybe req.urls (deleteAt idx) req.urls })

       Resource_ModState_SetTyResourceType source   -> do
         modify (\st->st{ currentResourceRequestSt = maybe Nothing (\rst -> Just $ rst{source = source}) st.currentResourceRequestSt }) $> next

       Save m_resource_id    -> do

         m_req <- gets _.currentResourceRequest

         case m_req of
              Nothing  -> eval (AddError "eval_Resource(Save)" "Resource request doesn't exist" next)
              Just req -> do

                case m_resource_id of
                     Nothing          -> do
                     -- Create a new resource
                       e_resource <- rd $ postResource' req
                       case e_resource of
                            Left err                          -> eval (AddErrorApi "eval_Resource(Save)::postResource'" err next)
                            Right (ResourceResponse resource) -> do
                              eval (Goto (Resources (ShowI resource.id) emptyParams) next)

                     Just resource_id -> do
                     -- Save an existing resource
                       e_resource <- rd $ putResource' resource_id req
                       case e_resource of
                            Left err                          -> eval (AddErrorApi "eval_Resource(Save)::putResource'" err next)
                            Right (ResourceResponse resource) -> do
                              eval (Goto (Resources (ShowI resource.id) emptyParams) next)

   InputResource_Nop   -> pure next

 where
 append :: forall a. Eq a => Maybe (Array a) -> a -> Maybe (Array a)
 append Nothing a    = Just [a]
 append (Just arr) a = Just $ nub $ arr <> [a]
 set' ref value request   = Just (_ResourceRequest .. ref .~ value $ request) -- unused.. this is nicer, but only typechecks for String
 set v req           = Just (v req)
 mod new             = modify (\st->st{ currentResourceRequest = maybe Nothing new st.currentResourceRequest }) $> next
