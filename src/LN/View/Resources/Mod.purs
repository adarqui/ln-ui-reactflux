module LN.View.Resources.Mod (
  renderView_Resources_Delete,
  renderView_Resources_New,
  renderView_Resources_Edit,
  renderView_Resources_Mod
) where



import Data.Maybe                      (Maybe(..), maybe)
import Data.Tuple                      (Tuple(..))
import Halogen                         (ComponentHTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Events             as E
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3       as B
import Optic.Core                      ((^.), (..))
import Prelude                         (id, map, show, const, ($), (<<<))

import LN.Halogen.Util
import LN.Helpers.Array                (seqArrayFrom)
import LN.Helpers.JSON                 (decodeString)
import LN.Internal.Resource            (resourceTypeToTyResourceType, unwrapResourceSource)
import LN.Input.Resource
import LN.Input.Types                  (Input(..), cResourceMod)
import LN.State.Loading                (getLoading, l_currentResource)
import LN.State.Resource               (ResourceRequestState)
import LN.State.Types                  (State)
import LN.View.Module.Loading          (renderLoading)
import LN.T



renderView_Resources_Delete :: Int -> State -> ComponentHTML Input
renderView_Resources_Delete resource_id st =

  case st.currentResource, getLoading l_currentResource st.loading of
       _, true          -> renderLoading
       Nothing, false   -> H.div_ [H.p_ [H.text "resource unavailable."]]
       Just pack, false -> renderView_Resources_Delete' pack st



renderView_Resources_Delete' :: ResourcePackResponse -> State -> ComponentHTML Input
renderView_Resources_Delete' pack st =
  H.div_ [H.p_ [H.text "Delete? <yes/no>"]]
 where
 resource = pack ^. _ResourcePackResponse .. resource_ ^. _ResourceResponse



renderView_Resources_New :: State -> ComponentHTML Input
renderView_Resources_New = renderView_Resources_Mod Nothing



renderView_Resources_Edit :: Int -> State -> ComponentHTML Input
renderView_Resources_Edit resource_id = renderView_Resources_Mod (Just resource_id)



renderView_Resources_Mod :: Maybe Int -> State -> ComponentHTML Input
renderView_Resources_Mod m_resource_id st =
  case st.currentResourceRequest, st.currentResourceRequestSt, getLoading l_currentResource st.loading of
    _, _, true                         -> renderLoading
    Just resource_req, Just rst, false -> renderView_Resources_Mod' m_resource_id resource_req rst st
    _, _, false                        -> H.div_ [H.p_ [H.text "Resources_Mod: unexpected error."]]



renderView_Resources_Mod' :: Maybe Int -> ResourceRequest -> ResourceRequestState -> State -> ComponentHTML Input
renderView_Resources_Mod' m_resource_id resource_req rst st =
  H.div_ [

    H.h1_ [ H.text "Add Resource" ]

  , input_Label "Name" "Name" resource.displayName P.InputText (E.input (cResourceMod <<< Resource_Mod_SetDisplayName))

  , textArea_Label "Description" "Description" resource.description (E.input (cResourceMod <<< Resource_Mod_SetDescription))

  --
  -- ResourceSource
  --

    , radioMenu "Resource Type" "resource-type" [TySourceNone, TyURL, TyISBN] (cResourceMod <<< Resource_ModState_SetTyResourceType) rst.source

    , case rst.source of
           TySourceNone -> H.p_ [H.text "NONE"]
           TyURL        -> H.p_ [input_Label "URL" "url" (unwrapResourceSource resource.source) P.InputUrl (E.input (\url -> cResourceMod $ Resource_Mod_SetSource (URL url)))]
           TyISBN       -> H.p_ [input_Label "ISBN" "isbn" (unwrapResourceSource resource.source) P.InputText (E.input (\isbn -> cResourceMod $ Resource_Mod_SetSource (ISBN isbn)))]

  --
  -- ResourceAuthor
  --

  , input_Label "Author" "Author" "" P.InputText  (E.input (cResourceMod <<< Resource_Mod_AddAuthor))

  , case resource.author of
         Nothing -> H.div_ []
         (Just authors) -> H.div_ $
            map (\(Tuple idx author) ->
              input_DeleteEdit
                P.InputText
                author
                (E.input (\new -> (cResourceMod $ Resource_Mod_EditAuthor idx new)))
                (E.input_ (cResourceMod $ Resource_Mod_DelAuthor idx))
              ) $ seqArrayFrom authors

  --
  -- ResourceCategories
  --

  , input_Label "Categories" "Category" "" P.InputText  (E.input (cResourceMod <<< Resource_Mod_AddCategory <<< maybe [] id <<< decodeString))

  , H.div_ $
      map (\(Tuple idx category) ->
        input_DeleteEdit
          P.InputText
          (show category)
          (E.input (\new -> cResourceMod $ Resource_Mod_EditCategory idx (maybe [] id $ decodeString new)))
          (E.input_ (cResourceMod $ Resource_Mod_DelCategory idx))
      ) $ seqArrayFrom resource.categories

  --
  -- ResourceVisibility
  --

  , radioMenu "Visibility" "resource-viz" [Public, Private] (cResourceMod <<< Resource_Mod_SetVisibility) resource.visibility

  , case resource.visibility of
         Public  -> H.p_ [H.text "Public"]
         Private -> H.p_ [H.text "Private"]

  --
  -- ResourceUrls
  --

  , input_Label "Urls" "Url" "" P.InputText  (E.input (cResourceMod <<< Resource_Mod_AddUrl))

  , case resource.urls of
         Nothing -> H.div_ []
         (Just urls) -> H.div_ $
            map (\(Tuple idx url) ->
              input_DeleteEdit
                P.InputUrl
                url
                (E.input (\new -> cResourceMod $ Resource_Mod_EditUrl idx new))
                (E.input_ (cResourceMod $ Resource_Mod_DelUrl idx))
              ) $ seqArrayFrom urls

 , simpleInfoButton save (cResourceMod $ Resource_Mod_Save m_resource_id)

  ]
  where
  resource = unwrapResourceRequest resource_req
  save     = maybe "Create" (const "Save") m_resource_id
