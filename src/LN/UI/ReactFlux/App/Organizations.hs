{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module LN.UI.ReactFlux.App.Organizations (
  viewIndex,
  viewNew,
  viewEditS,
  viewShowS
) where



import           Control.Concurrent                    (forkIO)
import           Control.DeepSeq                       (NFData)
import           Control.Monad                         (forM_, void)
import           Control.Monad.Trans.Either            (EitherT, runEitherT)
import           Data.Ebyam                            (ebyam)
import           Data.Map                              (Map)
import qualified Data.Map                              as Map
import           Data.Monoid                           ((<>))
import           Data.Rehtie                           (rehtie)
import           Data.Text                             (Text)
import           Data.Time                             (UTCTime)
import           Data.Typeable                         (Typeable)
import           GHC.Generics                          (Generic)
import           Haskell.Helpers.Either                (mustPassT)
import           React.Flux                            hiding (view)
import qualified React.Flux                            as RF
import qualified Web.Bootstrap3                        as B

import           LN.Api                                (getForumPacks_ByOrganizationId',
                                                        getOrganizationPacks,
                                                        getOrganizationsCount',
                                                        postOrganization',
                                                        putOrganization')
import           LN.Api.String                         (getOrganizationPack')
import           LN.Generate.Default                   (defaultOrganizationRequest)
import           LN.Sanitize.Internal                  (toSafeName)
import           LN.T.Convert                          (organizationResponseToOrganizationRequest)
import           LN.T.Organization                     (OrganizationRequest (..), OrganizationResponse (..), OrganizationResponse (..))
import           LN.T.Pack.Forum                       (ForumPackResponse (..),
                                                        ForumPackResponses (..))
import           LN.T.Pack.Organization                (OrganizationPackResponse (..), OrganizationPackResponses (..))
import           LN.T.Size                             (Size (..))
import           LN.T.User                             (UserSanitizedResponse (..))
import qualified LN.UI.Core.App.Organization           as Organization
import           LN.UI.Core.Helpers.DataList           (deleteNth)
import           LN.UI.Core.Helpers.DataText           (tshow)
import           LN.UI.Core.Helpers.DataTime           (prettyUTCTimeMaybe)
import           LN.UI.Core.Helpers.GHCJS
import           LN.UI.Core.Helpers.HaskellApiHelpers  (rd)
import           LN.UI.Core.Helpers.Map                (idmapFrom)
import           LN.UI.Core.PageInfo                   (PageInfo (..),
                                                        defaultPageInfo,
                                                        pageInfoFromParams,
                                                        paramsFromPageInfo)
import           LN.UI.Core.Router                     (CRUD (..), Params,
                                                        Route (..),
                                                        RouteWith (..),
                                                        TyCRUD (..),
                                                        emptyParams, linkName,
                                                        routeWith, routeWith')
import           LN.UI.ReactFlux.Access
import           LN.UI.ReactFlux.App.Core.Shared
import qualified LN.UI.ReactFlux.App.Delete            as Delete
import qualified LN.UI.ReactFlux.App.Forums            as Forums (viewIndex_)
import qualified LN.UI.ReactFlux.App.Gravatar          as Gravatar
import           LN.UI.ReactFlux.App.Loader            (Loader (..))
import qualified LN.UI.ReactFlux.App.Loader            as Loader
import qualified LN.UI.ReactFlux.App.NotFound          as NotFound
import           LN.UI.ReactFlux.App.PageNumbers       (runPageInfo)
import qualified LN.UI.ReactFlux.App.PageNumbers       as PageNumbers
import           LN.UI.ReactFlux.Helpers.ReactFluxDOM
import           LN.UI.ReactFlux.Helpers.ReactFluxView (defineViewWithSKey)
import           LN.UI.ReactFlux.Types
import           LN.UI.ReactFlux.View.Button
import           LN.UI.ReactFlux.View.Field
import           LN.UI.ReactFlux.View.Internal         (showTagsSmall)



viewIndex
  :: PageInfo
  -> Loader (Map OrganizationId OrganizationPackResponse)
  -> HTMLView_

viewIndex !page_info' !l_organizations' = do
  defineViewWithSKey "organizations-index-1" (page_info', l_organizations') $ \(page_info, l_organizations) -> do
    cldiv_ B.containerFluid $ do
      h1_ ["key" $= "organizations-index-1-h1"] "Organizations"
      ahrefKey "organizations-index-new" $ routeWith' $ Organizations New
      PageNumbers.view page_info (routeWith' $ Organizations Index)
      Loader.loader1 l_organizations (viewIndex_ page_info)



viewIndex_
  :: PageInfo
  -> Map OrganizationId OrganizationPackResponse
  -> HTMLView_

viewIndex_ !page_info' !organizations' = do
  defineViewWithSKey "organizations-index-2" (page_info', organizations') go
  where
  go (page_info, organizations) = do
    ul_ [className_ B.listUnstyled] $ do
      forM_ organizations viewIndexOrganization



viewIndexOrganization
  :: OrganizationPackResponse
  -> HTMLView_

viewIndexOrganization !org' =
  defineViewWithSKey ("organizations-index-org-" <> (showToJSString' $ organizationPackResponseOrganizationId org')) org' go
  where
  go :: OrganizationPackResponse -> HTMLView_
  go OrganizationPackResponse{..} = do
    let OrganizationResponse{..} = organizationPackResponseOrganization
    li_ ["key" @= organizationResponseId, className_ B.row] $ do
      viewUserGravatar B.colXs1 organizationPackResponseUser
      viewOrganizationLink B.colXs3 organizationPackResponseOrganization
      viewDescription B.colXs6 organizationResponseDescription
      viewUTCTimeMaybe B.colXs2 organizationResponseCreatedAt



viewUserGravatar :: JSString -> UserSanitizedResponse -> HTMLView_
viewUserGravatar !col' !user' = defineViewWithSKey "organization-user-gravatar" (col', user') go
  where
  go (col, user) = cldiv_ col $ Gravatar.viewUser XSmall user



viewOrganizationLink :: JSString -> OrganizationResponse -> HTMLView_
viewOrganizationLink !col' !org' = defineViewWithSKey "organization-link" (col', org') go
  where
  go (col, OrganizationResponse{..}) = cldiv_ col $ p_ $ ahrefName organizationResponseDisplayName (routeWith' $ Organizations (ShowS organizationResponseName))



viewDescription :: JSString -> Maybe Text -> HTMLView_
viewDescription !col' !m_desc' = defineViewWithSKey "description" (col', m_desc') go
  where
  go (col, m_desc) = cldiv_ col $ p_ $ elemText $ maybe "No Description." id m_desc



viewUTCTimeMaybe :: JSString -> Maybe UTCTime -> HTMLView_
viewUTCTimeMaybe !col' !m_utc' = defineViewWithSKey "maybeUTC" (col', m_utc') go
  where
  go (col, m_utc) = cldiv_ col $ p_ $ elemText $ prettyUTCTimeMaybe m_utc



viewShowS
  :: PageInfo
  -> Loader (Maybe OrganizationPackResponse)
  -> Loader (Map OrganizationId ForumPackResponse)
  -> HTMLView_

viewShowS !page_info' !l_m_organization' !l_forums' = do
  defineViewWithSKey "organizations-show-1" (page_info', l_m_organization', l_forums') $ \(page_info, l_m_organization, l_forums) -> do
    Loader.loader1 l_m_organization (go page_info l_forums)
    where
    go page_info l_forums (Just organization_pack@OrganizationPackResponse{..}) = do
      let OrganizationResponse{..} = organizationPackResponseOrganization
      cldiv_ B.containerFluid $ do
        viewShowHeader organizationPackResponseOrganization

        -- ACCESS: Organization
        -- * Member: if not a member, this is a shortcut to join an organization
        --
        isMemberOfOrganizationHTML
          organization_pack
          mempty
          -- (defineViewWithSKey "button-join" () $ const $ button_joinOrganization $ routeWith' $ OrganizationsMembership organizationResponseName Index)
          (defineViewWithSKey "button-join" () $ const $ button_joinOrganization' (dispatch JoinOrganization))

        -- ACCESS: Organization
        -- * Update: can edit organization settings
        -- * Delete: can delete organization
        --
        permissionsHTML'
          organizationPackResponsePermissions
          permCreateEmpty
          permReadEmpty
          (defineViewWithSKey "button-edit" () $ const $ button_editOrganization $ routeWith' $ Organizations (EditS organizationResponseName))
          (defineViewWithSKey "button-delete" () $ const $ button_deleteOrganization $ routeWith' $ Organizations (DeleteS organizationResponseName))
          permExecuteEmpty

        defineViewWithSKey "organizations-show-body" () $ \_ -> do
          h4_ $ do
            elemText "Name:"
            small_ ["key" $= "fixme"] $ elemText (" " <> organizationResponseName)

        ebyam organizationResponseDescription mempty $ \desc -> defineViewWithSKey "organizations-show-desc" () $ \_ -> do
          h4_ $ do
            elemText "Description: "
            small_ ["key" $= "fixme"] $ elemText desc

        defineViewWithSKey "organizations-show-company" organizationResponseCompany $ \company ->
          h4_ $ do
            elemText "Company"
            small_ ["key" $= "fixme"] $ elemText $ " " <> company

        defineViewWithSKey "organizations-show-location" organizationResponseLocation $ \location ->
          h4_ $ do
            elemText "Location"
            small_ ["key" $= "fixme"] $ elemText $ " " <> location

        defineViewWithSKey "organizations-show-membership" organizationResponseMembership $ \membership ->
          h4_ $ do
            elemText "Location"
            small_ ["key" $= "fixme"] $ elemText $ " " <> tshow membership

        defineViewWithSKey "organizations-show-visibility" organizationResponseVisibility $ \visibility ->
          h4_ $ do
            elemText "Location"
            small_ ["key" $= "fixme"] $ elemText $ " " <> tshow visibility

        defineViewWithSKey "organizations-show-tags" organizationResponseTags $ \tags ->
          h4_ $ do
            elemText "Tags: "
            showTagsSmall tags

        Loader.loader1 l_forums $ Forums.viewIndex_ page_info organization_pack

    go _ _ _ = NotFound.view



viewShowHeader
  :: OrganizationResponse
  -> HTMLView_
viewShowHeader !org' = defineViewWithSKey "organization-show-header" org' go
  where
  go OrganizationResponse{..} = do
    cldiv_ B.pageHeader $ do
      h1_ ["key" $= "organization-show-header-h1", className_ B.textCenter] $ elemText organizationResponseDisplayName
      p_ ["key" $= "organization-show-header-p", className_ B.textCenter] $ elemText $ maybe "" id organizationResponseDescription



viewNew
  :: Maybe OrganizationRequest
  -> HTMLView_

viewNew !m_request =
  ebyam m_request mempty $ \request -> viewMod TyCreate Nothing request



viewEditS
  :: Maybe OrganizationRequest
  -> Loader (Maybe OrganizationPackResponse)
  -> HTMLView_

viewEditS !m_request !l_organization_pack =
  Loader.loader1 l_organization_pack $ \m_organization_pack -> do
    case (m_request, m_organization_pack) of
      (Just request, Just OrganizationPackResponse{..}) -> viewMod TyUpdate (Just organizationPackResponseOrganizationId) request
      _ -> mempty



-- | Strictness requirement on input fields
--
viewMod
  :: TyCRUD
  -> Maybe OrganizationId
  -> OrganizationRequest
  -> HTMLView_

viewMod !tycrud' !m_organization_id' !request' = do
  defineViewWithSKey "organizations-mod-1" (tycrud', m_organization_id', request') $ \(tycrud, m_organization_id, request) -> do
    let
      OrganizationRequest{..} = request
    div_ $ do
      h1_ $ elemText $ linkName tycrud <> " Organization"

      mandatoryNameField organizationRequestDisplayName (dispatch . Organization.setDisplayName request)

      renderedText "Safe name: " (toSafeName organizationRequestDisplayName)

      optionalDescriptionField organizationRequestDescription
        (dispatch . Organization.setDescription request)
        (dispatch $ Organization.clearDescription request)

      mandatoryCompanyField organizationRequestCompany
        (dispatch . Organization.setCompany request)

      mandatoryLocationField organizationRequestLocation
        (dispatch . Organization.setLocation request)

      mandatoryMembershipField organizationRequestMembership
        (dispatch . Organization.setMembership request)

      mandatoryVisibilityField organizationRequestVisibility
        (dispatch . Organization.setVisibility request)

      tagsField
         organizationRequestTags
         (maybe ""  id organizationRequestStateTag)
         (dispatch . Organization.setTag request)
         (dispatch $ Organization.addTag request)
         (dispatch . Organization.deleteTag request)
         (dispatch $ Organization.clearTags request)

      createButtonsCreateEditCancel
        m_organization_id
        (dispatch Save)
        (const $ dispatch Save)
        (routeWith' Home)
