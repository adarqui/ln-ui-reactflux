module LN.Internal.Organization (
  defaultOrganizationRequest
) where



import Data.Generic                (class Generic)
import Data.Maybe                  (Maybe(Nothing))
import Prelude                     (class Eq, class Show)

import LN.T




defaultOrganizationRequest :: OrganizationRequest
defaultOrganizationRequest = mkOrganizationRequest "" Nothing "" "" "" Membership_Join [] Nothing Public 0
