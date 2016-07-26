{-# LANGUAGE RecordWildCards #-}

module LN.UI.Access (
  isMemberOfOrganization,
  isMemberOfOrganizationHTML,
  isMemberOfOrganizationHTML',
  permissionsHTML,
  permissionsHTML'
) where



import           React.Flux

import           LN.T
import           LN.UI.Types



-- | Check whether a user is a member of Team_Members
--
isMemberOfOrganization :: OrganizationPackResponse -> Bool
isMemberOfOrganization OrganizationPackResponse{..} = Team_Members `elem` organizationPackResponseTeams



-- | Supports handlers for if a user is/isnt a member
--
isMemberOfOrganizationHTML
  :: OrganizationPackResponse
  -> HTMLView_
  -> HTMLView_
  -> HTMLView_

isMemberOfOrganizationHTML pack is_member_handler isnt_member_handler =
  if isMemberOfOrganization pack
    then is_member_handler
    else isnt_member_handler



-- | Supports a handler if a user is a member
--
isMemberOfOrganizationHTML'
  :: OrganizationPackResponse
  -> HTMLView_
  -> HTMLView_

isMemberOfOrganizationHTML' pack is_member_handler =
  if isMemberOfOrganization pack
    then is_member_handler
    else mempty



permissionsHTML
  :: Permissions
  -> HTMLView_
  -> HTMLView_
  -> HTMLView_
  -> HTMLView_
  -> HTMLView_
  -> HTMLView_
  -> HTMLView_
  -> HTMLView_
  -> HTMLView_
  -> HTMLView_
  -> HTMLView_
permissionsHTML perms create_cb no_create_cb read_cb no_read_cb update_cb no_update_cb delete_cb no_delete_cb execute_cb no_execute_cb =
  div_ $ do
      if Perm_Create  `elem` perms then create_cb  else no_create_cb
      if Perm_Read    `elem` perms then read_cb    else no_read_cb
      if Perm_Update  `elem` perms then update_cb  else no_update_cb
      if Perm_Delete  `elem` perms then delete_cb  else no_delete_cb
      if Perm_Execute `elem` perms then execute_cb else no_execute_cb



permissionsHTML'
  :: Permissions
  -> HTMLView_
  -> HTMLView_
  -> HTMLView_
  -> HTMLView_
  -> HTMLView_
  -> HTMLView_
permissionsHTML' perms create_cb read_cb update_cb delete_cb execute_cb =
  permissionsHTML perms create_cb mempty read_cb mempty update_cb mempty delete_cb mempty execute_cb mempty
