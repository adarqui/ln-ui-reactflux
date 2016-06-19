module LN.Router.Class.Params (
  Params (..),
  PSRoutingParams,
  emptyParams,
  lookupParam,
  fixParams,
  psRoutingParamsToParams,
  paramTagFromString
) where



import Data.Int                    as I
import Data.List                   (catMaybes)
import Data.Maybe                  (Maybe(..), maybe)
import Data.Map                    as M
import Data.StrMap                 as StrM
import Data.Tuple                  (Tuple(..))
import Prelude                     (id, map, show, ($))

import LN.T                        (Param(..), ParamTag(..), OrderBy(..), SortOrderBy(..))



type Params = StrM.StrMap Param



type PSRoutingParams = M.Map String String



emptyParams :: Params
emptyParams = StrM.empty



lookupParam :: ParamTag -> Params -> Maybe Param
lookupParam p_tag params = StrM.lookup (show p_tag) params



fixParams :: Params -> Params
fixParams = id
-- fixParams :: Params -> PSRoutingParams
-- fixParams params = M.fromList $ map (qp <<< snd) $ M.toList params --  M.fromList <<< arrayToList
-- fixParams params = M.fromList $ map (qp <<< snd) $ M.toList params --  M.fromList <<< arrayToList



psRoutingParamsToParams :: PSRoutingParams -> Params
psRoutingParamsToParams ps =
  StrM.fromList $ catMaybes $ map (\(Tuple k v) -> (paramFromKV' k v)) $ M.toList ps



paramFromKV :: String -> String -> Maybe Param
paramFromKV k v = Nothing



paramFromKV' :: String -> String -> Maybe (Tuple String Param)
paramFromKV' k v =
  case paramTagFromString k of
    Nothing    -> Nothing
    Just ParamTag_Limit     -> maybe Nothing (\v -> Just $ Tuple k (Limit v)) (I.fromString v)
    Just ParamTag_Offset    -> maybe Nothing (\v -> Just $ Tuple k (Offset v)) (I.fromString v)
    Just ParamTag_Order     -> Just $ Tuple k (Order $ orderFromString v)
    Just ParamTag_SortOrder -> Just $ Tuple k (SortOrder $ sortOrderFromString v)
    Just _                  -> Nothing



paramTagFromString :: String -> Maybe ParamTag
paramTagFromString s =
  case s of
    "limit"                  ->  Just ParamTag_Limit
    "offset"                 ->  Just ParamTag_Offset
    "sort_order"             ->  Just ParamTag_SortOrder
    "order"                  ->  Just ParamTag_Order
    "organization_id"        ->  Just ParamTag_ByOrganizationId
    "organizations_ids"      ->  Just ParamTag_ByOrganizationsIds
    "organization_name"      ->  Just ParamTag_ByOrganizationName
    "team_id"                ->  Just ParamTag_ByTeamId
    "teams_ids"              ->  Just ParamTag_ByTeamsIds
    "team_name"              ->  Just ParamTag_ByTeamName
    "team_member_id"         ->  Just ParamTag_ByTeamMemberId
    "teams_member_ids"       ->  Just ParamTag_ByTeamMembersIds
    "user_id"                ->  Just ParamTag_ByUserId
    "users_ids"              ->  Just ParamTag_ByUsersIds
    "user_nick"              ->  Just ParamTag_ByUserNick
    "users_nicks"            ->  Just ParamTag_ByUsersNicks
    "global_group_id"        ->  Just ParamTag_ByGlobalGroupId
    "global_groups_ids"      ->  Just ParamTag_ByGlobalGroupsIds
    "group_id"               ->  Just ParamTag_ByGroupId
    "groups_ids"             ->  Just ParamTag_ByGroupsIds
    "group_member_id"        ->  Just ParamTag_ByGroupMemberId
    "groups_member_ids"      ->  Just ParamTag_ByGroupMembersIds
    "forum_id"               ->  Just ParamTag_ByForumId
    "forums_ids"             ->  Just ParamTag_ByForumsIds
    "forum_name"             ->  Just ParamTag_ByForumName
    "board_id"               ->  Just ParamTag_ByBoardId
    "boards_ids"             ->  Just ParamTag_ByBoardsIds
    "board_name"             ->  Just ParamTag_ByBoardName
    "thread_id"              ->  Just ParamTag_ByThreadId
    "threads_ids"            ->  Just ParamTag_ByThreadsIds
    "thread_name"            ->  Just ParamTag_ByThreadName
    "thread_post_id"         ->  Just ParamTag_ByThreadPostId
    "thread_posts_ids"       ->  Just ParamTag_ByThreadPostsIds
    "thread_post_name"       ->  Just ParamTag_ByThreadPostName
    "thread_post_like_id"    ->  Just ParamTag_ByThreadPostLikeId
    "thread_post_likes_ids"  ->  Just ParamTag_ByThreadPostLikesIds
    "thread_post_star_id"    ->  Just ParamTag_ByThreadPostStarId
    "thread_post_stars_ids"  ->  Just ParamTag_ByThreadPostStarsIds
    "bucket_id"              ->  Just ParamTag_ByBucketId
    "resource_id"            ->  Just ParamTag_ByResourceId
    "resources_ids"          ->  Just ParamTag_ByResourcesIds
    "resource_name"          ->  Just ParamTag_ByResourceName
    "leuron_id"              ->  Just ParamTag_ByLeuronId
    "leurons_ids"            ->  Just ParamTag_ByLeuronsIds
    "pm_id"                  ->  Just ParamTag_ByPmId
    "pms_ids"                ->  Just ParamTag_ByPmsIds
    "reminder_id"            ->  Just ParamTag_ByReminderId
    "reminder_folder_id"     ->  Just ParamTag_ByReminderFolderId
    "parent_id"              ->  Just ParamTag_ByParentId
    "parents_ids"            ->  Just ParamTag_ByParentsIds
    "parent_name"            ->  Just ParamTag_ByParentName
    "ts"                     ->  Just ParamTag_Timestamp
    "unix_ts"                ->  Just ParamTag_UnixTimestamp
    "created_at_ts"          ->  Just ParamTag_CreatedAtTimestamp
    "created_at_unix_ts"     ->  Just ParamTag_CreatedAtUnixTimestamp
    "real_ip"                ->  Just ParamTag_RealIP
    "ip"                     ->  Just ParamTag_IP
    _                        ->  Nothing



sortOrderFromString :: String -> SortOrderBy
sortOrderFromString s =
  case s of
    "asc"  -> SortOrderBy_Asc
    "dsc"  -> SortOrderBy_Dsc
    "rand" -> SortOrderBy_Rnd
    _      -> SortOrderBy_None



orderFromString :: String -> OrderBy
orderFromString s =
  case s of
    "user_id"     -> OrderBy_UserId
    "created_at"  -> OrderBy_CreatedAt
    "modified_at" -> OrderBy_ModifiedAt
    "modified_by" -> OrderBy_ModifiedBy
    "activity_at" -> OrderBy_ActivityAt
    "org_id"      -> OrderBy_OrganizationId
    "team_id"     -> OrderBy_TeamId
    "forum_id"    -> OrderBy_ForumId
    "board_id"    -> OrderBy_BoardId
    "thread_id"   -> OrderBy_ThreadId
    "id"          -> OrderBy_Id
    _             -> OrderBy_None
