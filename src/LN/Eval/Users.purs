module LN.Eval.Users (
  eval_GetUsers,
  eval_GetUser,
  eval_GetUsers_MergeMap_ByUser,
  eval_GetUsers_MergeMap_ByUserId
) where



import Halogen                       (gets, modify)
import Daimyo.Data.ArrayList         (arrayToList)
import Data.Array                    (nub, filter, zip)
import Data.Either                   (Either(..))
import Data.Map                      as M
import Data.Maybe                    (Maybe(..))
import Data.Tuple                    (Tuple(..))
import Optic.Core                    ((^.), (..))
import Prelude                       (bind, pure, not, map, ($))

import LN.Api                        (rd, getUsersCount' , getUserSanitizedPacks
                                     , getUserSanitizedPacks_ByUsersIds')
import LN.Api.Internal.String        as ApiS
import LN.Component.Types            (EvalEff)
import LN.Input.Types                (Input(..))
import LN.State.PageInfo             (runPageInfo)
import LN.T



eval_GetUsers :: EvalEff
eval_GetUsers eval (GetUsers next) = do

  page_info <- gets _.usersPageInfo

  ecount <- rd getUsersCount'
  case ecount of
    Left err -> pure next
    Right counts -> do

      let new_page_info = runPageInfo counts page_info

      modify (_{ usersPageInfo = new_page_info.pageInfo })

      eusers <- rd $ getUserSanitizedPacks new_page_info.params

      case eusers of
        Left err -> pure next
        Right (UserSanitizedPackResponses user_packs) -> do
          let
            users     = user_packs.userSanitizedPackResponses
            users_map = M.fromFoldable $ zip (map (\(UserSanitizedPackResponse pack) -> pack.user ^. _UserSanitizedResponse .. id_) users) users

          -- TODO FIXME: merge this with pre-existing users? union? correct?
          modify (\st -> st{ users = M.union st.users users_map })
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
