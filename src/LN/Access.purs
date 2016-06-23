module LN.Access (
  permissionsHTML,
  permissionsHTML',
  unitDiv,
  permCreateEmpty,
  permReadEmpty,
  permUpdateEmpty,
  permDeleteEmpty,
  permExecuteEmpty,
  ifte_Self,
  ifte_NotSelf,
  self,
  notSelf,
  ifte_OrgMember,
  orgMember,
  orgMemberHTML,
  orgMemberHTML',
  ifte_OrgOwner,
  orgOwner,
  orgOwnerHTML,
  orgOwnerHTML'
) where



-- import Data.Array                      (elem)
import Data.Foldable (elem)
import Halogen                         (ComponentHTML, HTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3       as B
import Prelude                         (Unit, unit, (==), (/=))

import LN.T




permissionsHTML
  :: Permissions
  -> (Unit -> HTML _ _)
  -> (Unit -> HTML _ _)
  -> (Unit -> HTML _ _)
  -> (Unit -> HTML _ _)
  -> (Unit -> HTML _ _)
  -> (Unit -> HTML _ _)
  -> (Unit -> HTML _ _)
  -> (Unit -> HTML _ _)
  -> (Unit -> HTML _ _)
  -> (Unit -> HTML _ _)
  -> HTML _ _
permissionsHTML perms create_cb no_create_cb read_cb no_read_cb update_cb no_update_cb delete_cb no_delete_cb execute_cb no_execute_cb =
  H.div_
    [
      if Perm_Create `elem` perms then (create_cb unit) else (no_create_cb unit),
      if Perm_Read `elem` perms then (read_cb unit) else (no_read_cb unit),
      if Perm_Update `elem` perms then (update_cb unit) else (no_update_cb unit),
      if Perm_Delete `elem` perms then (delete_cb unit) else (no_delete_cb unit),
      if Perm_Execute `elem` perms then (execute_cb unit) else (no_execute_cb unit)
    ]



permissionsHTML'
  :: Permissions
  -> (Unit -> HTML _ _)
  -> (Unit -> HTML _ _)
  -> (Unit -> HTML _ _)
  -> (Unit -> HTML _ _)
  -> (Unit -> HTML _ _)
  -> HTML _ _
permissionsHTML' perms create_cb read_cb update_cb delete_cb execute_cb =
  permissionsHTML perms create_cb unitDiv read_cb unitDiv update_cb unitDiv delete_cb unitDiv execute_cb unitDiv



unitDiv :: Unit -> HTML _ _
unitDiv _ = H.div_ []



permCreateEmpty  = unitDiv
permReadEmpty    = unitDiv
permUpdateEmpty  = unitDiv
permDeleteEmpty  = unitDiv
permExecuteEmpty = unitDiv



ifte_Self :: forall a. Int -> Int -> a -> a -> a
ifte_Self my_id questionable_id t e =
  if my_id == questionable_id
     then t
     else e



ifte_NotSelf :: forall a. Int -> Int -> a -> a -> a
ifte_NotSelf my_id questionable_id t e =
  if my_id /= questionable_id
     then t
     else e



self :: Int -> Int -> Boolean
self my_id questionable_id = my_id == questionable_id



notSelf :: Int -> Int -> Boolean
notSelf my_id questionable_id = my_id /= questionable_id



--
-- Owner Helpers
--

ifte_OrgOwner :: forall a. OrganizationPackResponse -> a -> a -> a
ifte_OrgOwner pack t e =
  if orgOwner pack
     then t
     else e



orgOwner :: OrganizationPackResponse -> Boolean
orgOwner (OrganizationPackResponse pack) = Team_Owners `elem` pack.teams



orgOwnerHTML :: OrganizationPackResponse -> (Unit -> HTML _ _) -> (Unit -> HTML _ _) -> HTML _ _
orgOwnerHTML pack owner_cb no_owner_cb =
  if orgOwner pack
    then owner_cb unit
    else no_owner_cb unit



orgOwnerHTML' :: OrganizationPackResponse -> (Unit -> HTML _ _) -> HTML _ _
orgOwnerHTML' pack owner_cb = orgOwnerHTML pack owner_cb unitDiv



--
-- Member helpers
--

ifte_OrgMember :: forall a. OrganizationPackResponse -> a -> a -> a
ifte_OrgMember pack t e =
  if orgMember pack
     then t
     else e



orgMember :: OrganizationPackResponse -> Boolean
orgMember (OrganizationPackResponse pack) = Team_Members `elem` pack.teams



orgMemberHTML :: OrganizationPackResponse -> (Unit -> HTML _ _) -> (Unit -> HTML _ _) -> HTML _ _
orgMemberHTML pack member_cb no_member_cb =
  if orgMember pack
    then member_cb unit
    else no_member_cb unit



orgMemberHTML' :: OrganizationPackResponse -> (Unit -> HTML _ _) -> HTML _ _
orgMemberHTML' pack member_cb = orgMemberHTML pack member_cb unitDiv
