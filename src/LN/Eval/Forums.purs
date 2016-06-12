module LN.Eval.Forums (
  eval_Forum
) where



import Data.Array                      (head, deleteAt, modifyAt, nub)
import Data.Either                     (Either(..))
import Data.Functor                    (($>))
import Data.Map                        as M
import Data.Maybe                      (Maybe(..), maybe)
import Halogen                         (gets, modify)
import Optic.Core                      ((^.), (..), (.~))
import Prelude                         (class Eq, bind, pure, ($), (<>), (<$>))

import LN.Api                          (rd, getForumPacks_ByOrganizationName', getForumPack', postForum_ByOrganizationId', putForum', getForumPacks_ByOrganizationId')
import LN.Api.Internal.String          as ApiS
import LN.Component.Types              (EvalEff)
import LN.Input.Forum                  (InputForum(..), Forum_Act(..), Forum_Mod(..))
import LN.Input.Types                  (Input(..))
import LN.Helpers.Map                  (idmapFrom)
import LN.Router.Class.Routes          (Routes(..))
import LN.Router.Class.CRUD            (CRUD(..))
import LN.State.Loading                (l_currentForum)
import LN.State.Loading.Helpers        (setLoading, clearLoading)
import LN.T.Internal.Convert           (forumResponseToForumRequest)
import LN.T                            ( ForumPackResponses(..), ForumPackResponse(..)
                                       , ForumResponse(..)
                                       , _ForumResponse, name_
                                       , ForumRequest(..)
                                       , _ForumRequest, displayName_, description_, icon_, tags_, visibility_, guard_
                                       , _OrganizationPackResponse, _OrganizationResponse, organization_, name_, organizationId_)



eval_Forum :: EvalEff
eval_Forum eval (CompForum sub next) = do

  case sub of

    InputForum_Act q -> do
      case q of
        Gets                                   -> pure next
        Gets_ByOrganizationId org_id           -> pure next
        Gets_ByCurrentOrganization             -> act_gets_by_current_organization
        GetId forum_id                         -> pure next
        GetSid_ByCurrentOrganization forum_sid -> act_get_sid_by_current_organization forum_sid



    InputForum_Mod q -> do
      case q of
        SetDisplayName name -> mod $ set (\req -> _ForumRequest .. displayName_ .~ name $ req)

        SetDescription s    -> mod $ set (\req -> _ForumRequest .. description_ .~ Just s $ req)
        RemoveDescription   -> mod $ set (\req -> _ForumRequest .. description_ .~ Nothing $ req)

        SetIcon s           -> mod $ set (\req -> _ForumRequest .. icon_ .~ Just s $ req)
        RemoveIcon          -> mod $ set (\req -> _ForumRequest .. icon_ .~ Nothing $ req)
        Create org_id       -> mod_create org_id
        EditP forum_id      -> mod_edit forum_id

    _   -> pure next

 where
  append :: forall a. Eq a => Maybe (Array a) -> a -> Maybe (Array a)
  append Nothing a    = Just [a]
  append (Just arr) a = Just $ nub $ arr <> [a]
  set v req           = Just (v req)
  mod new             = modify (\st->st{ currentForumRequest = maybe Nothing new st.currentForumRequest }) $> next

  get_org_name        = (maybe "unknown" (\org -> org ^. _OrganizationPackResponse .. organization_ ^. _OrganizationResponse .. name_)) <$> gets _.currentOrganization



  act_gets_by_current_organization = do
    modify (_{ forums = (M.empty :: M.Map Int ForumPackResponse) })
    m_org_pack <- gets _.currentOrganization
    case m_org_pack of
      Nothing       -> eval (AddError "eval_Forum(Act/Gets)" "Organization doesn't exist" next)
      Just org_pack -> do
        e_forum_packs <- rd $ getForumPacks_ByOrganizationId' (org_pack ^. _OrganizationPackResponse .. organizationId_)
        case e_forum_packs of
          Left err                               -> eval (AddErrorApi "eval_Forum(Act/Gets)::getForumPacks_ByOrgName'" err next)
          Right (ForumPackResponses forum_packs) -> do

            let
              forums_map = idmapFrom (\(ForumPackResponse pack) -> pack.forumId) forum_packs.forumPackResponses
            modify (_{ forums = forums_map })
            pure next



  act_get_sid_by_current_organization forum_sid = do
    modify (_{ currentForum = Nothing })
    m_org_pack <- gets _.currentOrganization
    case m_org_pack of
      Nothing       -> eval (AddError "eval_Forum(Act/Get)" "Organization doesn't exist" next)
      Just org_pack -> do
        e_forum_pack <- rd $ ApiS.getForumPack_ByOrganizationId' forum_sid (org_pack ^. _OrganizationPackResponse .. organizationId_)
        case e_forum_pack of
          Left err         -> eval (AddErrorApi "eval_Forum(Act/Get)::getForumPacks_ByOrgName'" err next)
          Right forum_pack -> do
            modify (_{ currentForum = Just forum_pack })
            pure next



  mod_create org_id = do
    org_name <- get_org_name
    m_req <- gets _.currentForumRequest
    case m_req of
         Just req -> do
           e_forum <- rd $ postForum_ByOrganizationId' org_id req
           case e_forum of
                Left err                    -> eval (AddErrorApi "eval_Forum(Create)::postForum'" err next)
                Right (ForumResponse forum) -> eval (Goto (OrganizationsForums org_name (Show forum.name) []) next)
         _        -> eval (AddError "eval_Forum(Create)" "Forum request doesn't exist" next)



  mod_edit forum_id = do
    org_name <- get_org_name
    m_req <- gets _.currentForumRequest
    case m_req of
         Nothing  -> eval (AddError "eval_Forum(Edit)" "Forum request doesn't exist" next)
         Just req -> do
           e_forum <- rd $ putForum' forum_id req
           case e_forum of
                Left err    -> eval (AddErrorApi "eval_Forum(Edit)::putForum" err next)
                Right forum -> do
                  modify (\st->st{ currentForumRequest = Just $ forumResponseToForumRequest forum })
                  eval (Goto (OrganizationsForums org_name (Show $ forum ^. _ForumResponse .. name_) []) next)
                  pure next
