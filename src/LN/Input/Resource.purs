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
  = SetDisplayName String
  | SetDescription String
  | SetSource ResourceType
  | AddAuthor String
  | DeleteAuthor Int
  | EditAuthor Int String
  | AddCategory (Array String)
  | DeleteCategory Int
  | EditCategory Int (Array String)
  | SetVisibility Visibility
  | AddUrl String
  | DeleteUrl Int
  | EditUrl Int String
  | Save (Maybe Int)
  | Resource_ModState_SetTyResourceType TyResourceType
