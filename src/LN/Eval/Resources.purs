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
                                     , postResource')
import LN.Component.Types            (EvalEff)
import LN.Helpers.Map                (idmapFrom)
import LN.Input.Resource             (InputResource(..), Resource_Mod(..))
import LN.Input.Types                (Input(..))
import LN.Router.Types               (Routes(..), CRUD(..))
import LN.State.Loading              (setLoading, clearLoading, l_currentLeuron)
import LN.State.PageInfo             (runPageInfo)
import LN.T                          ( Param(..), SortOrderBy(..)
                                     , ResourcePackResponses(..), ResourcePackResponse(..)
                                     , ResourceRequest(..), ResourceResponse(..)
                                     , _ResourceRequest
                                     , title_, description_, source_, visibility_
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

      e_resource_packs <- rd $ getResourcePacks new_page_info.params
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

  e_pack <- rd $ getResourcePack' resource_id
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
  modify (\st->st{loading = setLoading l_currentLeuron st.loading})

  e_packs <- rd $ getLeuronPacks_ByResourceId [Limit 1, Offset offset, SortOrder SortOrderBy_Asc] resource_id

  modify (\st->st{loading = clearLoading l_currentLeuron st.loading})

  case e_packs of
    Left err                          -> eval (AddErrorApi "eval_GetResourceLeuronLinear::getLeuronPacks_ByResourceId" err next)
    Right (LeuronPackResponses packs) -> do
      case head packs.leuronPackResponses of
        Nothing   -> pure next
        Just pack -> modify (_{ currentLeuron = Just pack }) $> next



eval_GetResourceLeuronRandom :: EvalEff
eval_GetResourceLeuronRandom eval (GetResourceLeuronRandom resource_id next) = do

  modify (_{ currentLeuron = Nothing })
  modify (\st->st{loading = setLoading l_currentLeuron st.loading})

  e_packs <- rd $ getLeuronPacks_ByResourceId [Limit 1, SortOrder SortOrderBy_Rnd] resource_id

  modify (\st->st{loading = clearLoading l_currentLeuron st.loading})

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
       Resource_Mod_SetTitle title        -> mod $ set (\req -> _ResourceRequest .. title_ .~ title $ req)

       Resource_Mod_SetDescription desc   -> mod $ set (\req -> _ResourceRequest .. description_ .~ desc $ req)

       Resource_Mod_SetSource source      -> mod $ set (\req -> _ResourceRequest .. source_ .~ source $ req)

       Resource_Mod_AddAuthor author'     -> mod $ set (\(ResourceRequest req) -> ResourceRequest req{ author = append req.author author' })

       Resource_Mod_DelAuthor idx         -> mod $ set (\(ResourceRequest req) -> ResourceRequest req{ author = maybe req.author (deleteAt idx) req.author })

       Resource_Mod_EditAuthor idx author -> mod $ set (\(ResourceRequest req) -> ResourceRequest req { author = maybe req.author (modifyAt idx (const author)) req.author })

       Resource_Mod_AddCategory cat       -> mod $ set (\(ResourceRequest req) -> ResourceRequest req{ categories = req.categories <> [cat] })

       Resource_Mod_DelCategory idx       -> mod $ set (\(ResourceRequest req) -> ResourceRequest req{ categories = maybe req.categories id $ deleteAt idx req.categories })

       Resource_Mod_EditCategory idx cat  -> mod $ set (\(ResourceRequest req) -> ResourceRequest req { categories = maybe req.categories id $ modifyAt idx (const cat) req.categories })

       Resource_Mod_SetVisibility viz     -> mod $ set (\req -> _ResourceRequest ..  visibility_ .~ viz $ req)

       Resource_Mod_AddUrl url            -> mod $ set (\(ResourceRequest req) -> ResourceRequest req{ urls = append req.urls url })

       Resource_Mod_DelUrl idx            -> mod $ set (\(ResourceRequest req) -> ResourceRequest req{ author = maybe req.urls (deleteAt idx) req.urls })

       Resource_Mod_EditUrl idx url       -> mod $ set (\(ResourceRequest req) -> ResourceRequest req{ urls = maybe req.urls (deleteAt idx) req.urls })

       Resource_ModState_SetRType rtype   -> do
         modify (\st->st{ currentResourceRequestSt = maybe Nothing (\rst -> Just $ rst{rtype = rtype}) st.currentResourceRequestSt }) $> next

       Resource_Mod_Save m_resource_id    -> do
         m_req <- gets _.currentResourceRequest
         case m_req of
              Nothing  -> eval (AddError "eval_Resource(Save)" "Resource request doesn't exist" next)
              Just req -> do
                e_resource <- rd $ postResource' req
                case e_resource of
                     Left err                          -> eval (AddErrorApi "eval_Resource(Save)::postResource'" err next)
                     Right (ResourceResponse resource) -> do
                       eval (Goto (Resources (ShowI resource.id) []) next)




   InputResource_Nop   -> pure next
 where
 append :: forall a. Eq a => Maybe (Array a) -> a -> Maybe (Array a)
 append Nothing a    = Just [a]
 append (Just arr) a = Just $ nub $ arr <> [a]
 set' ref value request   = Just (_ResourceRequest .. ref .~ value $ request) -- unused.. this is nicer, but only typechecks for String
 set v req                = Just (v req)
 mod new                  = modify (\st->st{ currentResourceRequest = maybe Nothing new st.currentResourceRequest }) $> next
