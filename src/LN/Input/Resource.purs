module LN.Input.Resource (
  InputResource(..),
  Resource_Mod(..)
) where



import Data.Maybe           (Maybe)

import LN.T                 (ResourceType, TyResourceType, Visibility)



-- | Mod = New or Edit


data InputResource
  = InputResource_Mod Resource_Mod
  | InputResource_Nop



data Resource_Mod
  = Resource_Mod_SetTitle String
  | Resource_Mod_SetDescription String
  | Resource_Mod_SetSource ResourceType
  | Resource_Mod_AddAuthor String
  | Resource_Mod_DelAuthor Int
  | Resource_Mod_EditAuthor Int String
  | Resource_Mod_AddCategory (Array String)
  | Resource_Mod_DelCategory Int
  | Resource_Mod_EditCategory Int (Array String)
  | Resource_Mod_SetVisibility Visibility
  | Resource_Mod_AddUrl String
  | Resource_Mod_DelUrl Int
  | Resource_Mod_EditUrl Int String
  | Resource_Mod_Save (Maybe Int)
  | Resource_ModState_SetTyResourceType TyResourceType
