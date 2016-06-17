module LN.Router.Class.CRUD (
  CRUD (..),
  TyCRUD (..)
) where



import Control.Monad.Aff           (Aff())
import Control.Monad.Aff.AVar      (AVAR())
import Control.Monad.Eff.Exception (EXCEPTION())
import Data.Generic                (class Generic, gEq)
import Data.Map                    as M
import Data.Tuple                  (Tuple(..), fst)
import DOM                         (DOM())
import LN.Router.Util              (slash, fixParams)
import Prelude                     (class Eq, class Show, show, (<>), ($), (++), (==))

import LN.T                        (OrderBy(..))
import LN.Router.Class.Link



data CRUD
  = Index
  | Show  String
  | ShowI Int
  | ShowN Number
  | ShowB Boolean
  | New
  | Edit String
  | EditI Int
  | EditN Number
  | Delete String
  | DeleteI Int
  | DeleteN Number



derive instance genericCRUD :: Generic CRUD



instance eqCrud :: Eq CRUD where
  eq Index        Index        = true
  eq (Show a)     (Show b)     = a == b
  eq (ShowI a)    (ShowI b)    = a == b
  eq (ShowN a)    (ShowN b)    = a == b
  eq (ShowB a)    (ShowB b)    = a == b
  eq New          New          = true
  eq (Edit a)     (Edit b)     = a == b
  eq (EditI a)    (EditI b)    = a == b
  eq (EditN a)    (EditN b)    = a == b
  eq (Delete a)   (Delete b)   = a == b
  eq (DeleteI a)  (DeleteI b)  = a == b
  eq (DeleteN a)  (DeleteN b)  = a == b
  eq _            _            = false



instance crudHasLink :: HasLink CRUD where
-- TODO FIXME:
-- well this could be fixed.. changed from "" in order to match CRUD Index routes
--  link Index    = Tuple "/index" M.empty
  link Index          = Tuple "" M.empty
  link New            = Tuple "/new" M.empty
  link (Show s)       = Tuple ("/" <> s) M.empty
  link (ShowI int)    = Tuple ("/" <> show int) M.empty
  link (ShowN num)    = Tuple ("/" <> show num) M.empty
  link (ShowB bool)   = Tuple ("/" <> show bool) M.empty
  link (Edit s)       = Tuple ("/_edit/" <> s) M.empty
  link (EditI int)    = Tuple ("/_edit/" <> show int) M.empty
  link (EditN num)    = Tuple ("/_edit/" <> show num) M.empty
  link (Delete s)     = Tuple ("/_delete/" <> s) M.empty
  link (DeleteI int)  = Tuple ("/_delete/" <> show int) M.empty
  link (DeleteN num)  = Tuple ("/_delete/" <> show num) M.empty



data TyCRUD
  = TyCreate
  | TyEdit
  | TyDelete
  | TyView



instance tyCRUDEq :: Eq TyCRUD where
  eq TyCreate TyCreate = true
  eq TyEdit   TyEdit   = true
  eq TyDelete TyDelete = true
  eq TyView   TyView   = true
  eq _        _        = false



instance tyCRUDShow :: Show TyCRUD where
  show TyCreate = "Create"
  show TyEdit   = "Edit"
  show TyDelete = "Delete"
  show TyView   = "View"
