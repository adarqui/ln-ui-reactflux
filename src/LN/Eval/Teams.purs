module LN.Eval.Teams (
  eval_Team
) where



import Data.Array                      (head, deleteAt, modifyAt, nub, sort, (:))
import Data.Ebyam                      (ebyam)
import Data.Either                     (Either(..))
import Data.Functor                    (($>))
import Data.Map                        as M
import Data.Maybe                      (Maybe(..), maybe)
import Data.String                     (toLower)
import Halogen                         (gets, modify)
import Optic.Core                      ((^.), (..), (.~))
import Prelude                         (class Eq, id, bind, pure, const, ($), (<>), (<$>), (<<<))

import LN.Api                          (rd, putTeam')
import LN.Api.Internal.String          as ApiS
import LN.Component.Types              (EvalEff)
import LN.Helpers.Map                  (idmapFrom)
import LN.Input.Team                   (InputTeam(..), Team_Act(..), Team_Mod(..))
import LN.Input.Types                  (Input(..))
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.Router.Class.Params          (emptyParams)
import LN.State.Loading                (l_currentTeam, l_teams)
import LN.State.Loading.Helpers        (setLoading, clearLoading)
import LN.State.Team                   (TeamRequestState, defaultTeamRequestState)
import LN.T.Internal.Convert           (teamResponseToTeamRequest)
import LN.T                            ( OrganizationPackResponses(..), OrganizationPackResponse(..)
                                       , OrganizationResponse(..)
                                       , _TeamPackResponse, TeamPackResponse(..)
                                       , _TeamResponse, TeamResponse(..)
                                       , _TeamRequest, TeamRequest(..)
                                       , membership_, visibility_)



eval_Team :: EvalEff
eval_Team eval (CompTeam sub next) = do

  m_me <- gets _.me

  case sub of

    InputTeam_Act q -> do
      case q of
        Gets_ByCurrentOrganization            -> pure next
        GetId team_id                         -> pure next
        GetSid_ByCurrentOrganization team_sid -> pure next



    InputTeam_Mod q -> do
      case q of
        SetMembership m      -> mod $ set (\req -> _TeamRequest .. membership_ .~ m $ req)
        SetVisibility viz    -> mod $ set (\req -> _TeamRequest .. visibility_ .~ viz $ req)
        EditP org_id         -> mod_edit org_id

    _   -> pure next

 where
  append :: forall a. Eq a => Maybe (Array a) -> a -> Maybe (Array a)
  append Nothing a    = Just [a]
  append (Just arr) a = Just $ nub $ arr <> [a]
  set v req           = Just (v req)
  mod new             = modify (\st->st{ currentTeamRequest = maybe Nothing new st.currentTeamRequest }) $> next

  mod_edit team_id = do
    m_req <- gets _.currentTeamRequest
    case m_req of
         Nothing  -> eval (AddError "eval_Team(Mod/Edit)" "Team request doesn't exist" next)
         Just req -> do
           e_team <- rd $ putTeam' team_id req
           case e_team of
                Left err  -> eval (AddErrorApi "eval_Team(Mod/Edit)::putTeam" err next)
                Right team -> do
                  modify (\st->st{ currentTeamRequest = Just $ teamResponseToTeamRequest team })
                  eval (Goto (OrganizationsTeams "test" (Show "test") emptyParams) next)
--                  eval (Goto (OrganizationsTeams "test" (Show $ team ^. _TeamResponse .. name_) emptyParams) next)
                  pure next
