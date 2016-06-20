module LN.Access (
  ifte_OrgOwner,
  ifte_OrgMember,
  ifte_Self,
  ifte_NotSelf,
  orgOwner,
  orgMember,
  self,
  notSelf
) where



import Prelude ((==), (/=))

import LN.T    (OrganizationPackResponse(..))



ifte_OrgOwner :: forall a. OrganizationPackResponse -> a -> a -> a
ifte_OrgOwner (OrganizationPackResponse pack) t e =
  if pack.isOwner == true
     then t
     else e



ifte_OrgMember :: forall a. OrganizationPackResponse -> a -> a -> a
ifte_OrgMember (OrganizationPackResponse pack) t e =
  if pack.isMember == true
     then t
     else e



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



orgOwner :: OrganizationPackResponse -> Boolean
orgOwner (OrganizationPackResponse pack) = pack.isOwner



orgMember :: OrganizationPackResponse -> Boolean
orgMember (OrganizationPackResponse pack) = pack.isMember



self :: Int -> Int -> Boolean
self my_id questionable_id = my_id == questionable_id



notSelf :: Int -> Int -> Boolean
notSelf my_id questionable_id = my_id /= questionable_id
