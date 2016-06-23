module LN.Eval.TeamMembers (
  eval_TeamMember
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

import LN.Api                          (rd, putTeamMember')
import LN.Api.Internal.String          as ApiS
import LN.Component.Types              (EvalEff)
import LN.Helpers.Map                  (idmapFrom)
import LN.Input.TeamMember                   (InputTeamMember(..), TeamMember_Act(..), TeamMember_Mod(..))
import LN.Input.Types                  (Input(..))
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.Router.Class.Params          (emptyParams)
import LN.State.Loading                (l_currentTeamMember, l_teams)
import LN.State.Loading.Helpers        (setLoading, clearLoading)
import LN.State.TeamMember                   (TeamMemberRequestState, defaultTeamMemberRequestState)
import LN.T.Internal.Convert           (teamResponseToTeamMemberRequest)
import LN.T                            ( OrganizationPackResponses(..), OrganizationPackResponse(..)
                                       , OrganizationResponse(..)
                                       , _TeamMemberPackResponse, TeamMemberPackResponse(..)
                                       , _TeamMemberResponse, TeamMemberResponse(..)
                                       , _TeamMemberRequest, TeamMemberRequest(..)
                                       , membership_, visibility_)



eval_TeamMember :: EvalEff
eval_TeamMember eval (CompTeamMember sub next) = do

  m_me <- gets _.me

  case sub of

    InputTeamMember_Act q -> do
      case q of
        Gets_ByCurrentTeam   -> pure next
        GetId team_member_id -> pure next



    InputTeamMember_Mod q -> do
      case q of
        _ -> pure next

    _   -> pure next

 where
  append :: forall a. Eq a => Maybe (Array a) -> a -> Maybe (Array a)
  append Nothing a    = Just [a]
  append (Just arr) a = Just $ nub $ arr <> [a]
  set v req           = Just (v req)
  mod new             = modify (\st->st{ currentTeamMemberRequest = maybe Nothing new st.currentTeamMemberRequest }) $> next
