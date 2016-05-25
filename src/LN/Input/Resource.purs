module LN.Input.Resource (
  InputResource (..)
) where



import Data.Maybe           (Maybe)

import LN.Internal.Resource (RType)
import LN.T                 (ResourceType, Visibility)



-- | Mod = New or Edit


data InputResource
  = InputResource_Mod_SetTitle String
  | InputResource_Mod_SetDescription String
  | InputResource_Mod_SetSource ResourceType
  | InputResource_Mod_AddAuthor String
  | InputResource_Mod_DelAuthor Int
  | InputResource_Mod_EditAuthor Int String
  | InputResource_Mod_AddCategory (Maybe (Array String))
  | InputResource_Mod_DelCategory Int
  | InputResource_Mod_EditCategory Int (Maybe (Array String))
  | InputResource_Mod_SetVisibility Visibility
  | InputResource_Mod_AddUrl String
  | InputResource_Mod_EditUrl Int String
  | InputResource_Mod_DelUrl Int
  | InputResource_Mod_Save (Maybe Int)
  | InputResource_ModState_SetRType RType

  | InputResource_Nop
