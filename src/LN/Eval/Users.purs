module LN.Eval.Users (
  eval_GetUsers,
  eval_GetUser,
  eval_GetUsers_MergeMap_ByUser,
  eval_GetUsers_MergeMap_ByUserId
) where



import Halogen                       (get, gets, modify)
import Daimyo.Data.ArrayList         (listToArray, arrayToList)
import Data.Array                    (nub, filter, head)
import Data.Either                   (Either(..))
import Data.Map                      as M
import Data.Maybe                    (Maybe(..), maybe)
import Data.Tuple                    (Tuple(..))
import Optic.Core                    ((^.), (..))
import Prelude                       (bind, pure, not, map, ($), (+), (*), (-), (/))

import LN.Api                        (rd, getUsersCount', getUserSanitizedPack'
                                     , getUserSanitizedPacks, getUserSanitizedPacks_ByUsersIds')
import LN.Api.Internal.String        as ApiS
import LN.Component.Types            (EvalEff)
import LN.Input.Types                (Input(..))
import LN.T



eval_GetUsers :: EvalEff
eval_GetUsers eval (GetUsers next) = do

  pageInfo <- gets _.usersPageInfo

  ecount <- rd getUsersCount'
  case ecount of
    Left err -> pure next
    Right (CountResponses counts) -> do

      let count = maybe 0 (\(CountResponse count) -> count.n) (head counts.countResponses)

      modify (_{ usersPageInfo = pageInfo { totalResults = count, totalPages = (count / pageInfo.resultsPerPage)+1 } })

      eusers <- rd $ getUserSanitizedPacks [Limit pageInfo.resultsPerPage, Offset ((pageInfo.currentPage-1) * pageInfo.resultsPerPage), SortOrder pageInfo.sortOrder]

      case eusers of
        Left err -> pure next
        Right (UserSanitizedPackResponses users) -> do
          modify (_{ users = users.userSanitizedPackResponses })
          pure next



eval_GetUser :: EvalEff
eval_GetUser eval (GetUser user_nick next) = do

  euser <- rd $ ApiS.getUserSanitizedPack' user_nick
  case euser of
      Left err -> pure next
      Right user -> do
        modify (_{ currentUser = Just user })
        pure next



-- | Takes an array of sanitized users, and pulls down any of them that
-- don't already exist in the current st.usersMap.
--
eval_GetUsers_MergeMap_ByUser :: EvalEff
eval_GetUsers_MergeMap_ByUser eval (GetUsers_MergeMap_ByUser users next) = do

  let
    users_ids = map (\user -> user ^. _UserSanitizedResponse .. id_) users

  eval_GetUsers_MergeMap_ByUserId eval (GetUsers_MergeMap_ByUserId users_ids next)




-- | Takes an array of users ids, and pulls down any of them that
-- don't already exist in the current st.usersMap.
--
eval_GetUsers_MergeMap_ByUserId :: EvalEff
eval_GetUsers_MergeMap_ByUserId eval (GetUsers_MergeMap_ByUserId users_ids next) = do

  usersMap <- gets _.usersMap

  let
    users_ids_not_in_map =
      filter (\user_id -> not $ M.member user_id usersMap)
      $ nub users_ids

  eresult <- rd $ getUserSanitizedPacks_ByUsersIds' users_ids_not_in_map

  case eresult of
       Left err -> pure next
       Right (UserSanitizedPackResponses result) -> do
         let
          newUsersMap =
            M.fromList
            $ arrayToList
            $ map (\user -> Tuple (user ^. _UserSanitizedPackResponse .. user_ ^. _UserSanitizedResponse .. id_) user) result.userSanitizedPackResponses

         modify (_{ usersMap = (M.union newUsersMap usersMap) })
         pure next
