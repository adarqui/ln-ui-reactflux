module LN.Router.Class.CRUD (
  CRUD (..),
  TyCRUD (..)
) where



-- import Data.Generic                (class Generic, gEq)
import Data.Map                    as M
import Data.Tuple                  (Tuple(..), fst)
import Prelude                     (class Eq, class Show, show, (<>), ($), (++), (==))

import LN.Router.Class.Link        (class HasLink)
import LN.Router.Class.Params      (emptyParams)



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
  | DeleteZ



instance crudEq :: Eq CRUD where
  eq Index        Index        = true
  eq (Show t1)    (Show t2)    = t1 == t2
  eq (ShowI t1)   (ShowI t2)   = t1 == t2
  eq (ShowN t1)   (ShowN t2)   = t1 == t2
  eq (ShowB t1)   (ShowB t2)   = t1 == t2
  eq New          New          = true
  eq (Edit t1)    (Edit t2)    = t1 == t2
  eq (EditI t1)   (EditI t2)   = t1 == t2
  eq (EditN t1)   (EditN t2)   = t1 == t2
  eq (Delete t1)  (Delete t2)  = t1 == t2
  eq (DeleteI t1) (DeleteI t2) = t1 == t2
  eq (DeleteN t1) (DeleteN t2) = t1 == t2
  eq DeleteZ      DeleteZ      = true
  eq _            _            = false



instance crudShow :: Show CRUD where
  show Index       = "Index"
  show (Show s)    = "Show " <> s
  show (ShowI i)   = "ShowI " <> show i
  show (ShowN n)   = "ShowN " <> show n
  show (ShowB b)   = "ShowB " <> show b
  show New         = "New"
  show (Edit s)    = "Edit " <> s
  show (EditI i)   = "EditI " <> show i
  show (EditN n)   = "EditN " <> show n
  show (Delete s)  = "Delete " <> show s
  show (DeleteI i) = "DeleteI " <> show i
  show (DeleteN n) = "DeleteN " <> show n
  show DeleteZ     = "DeleteZ"



instance crudHasLink :: HasLink CRUD where
-- TODO FIXME:
-- well this could be fixed.. changed from "" in order to match CRUD Index routes
--  link Index    = Tuple "/index" emptyParams
  link Index          = Tuple "" emptyParams
  link New            = Tuple "/new" emptyParams
  link (Show s)       = Tuple ("/" <> s) emptyParams
  link (ShowI int)    = Tuple ("/" <> show int) emptyParams
  link (ShowN num)    = Tuple ("/" <> show num) emptyParams
  link (ShowB bool)   = Tuple ("/" <> show bool) emptyParams
  link (Edit s)       = Tuple ("/_edit/" <> s) emptyParams
  link (EditI int)    = Tuple ("/_edit/" <> show int) emptyParams
  link (EditN num)    = Tuple ("/_edit/" <> show num) emptyParams
  link (Delete s)     = Tuple ("/_delete/" <> s) emptyParams
  link (DeleteI int)  = Tuple ("/_delete/" <> show int) emptyParams
  link (DeleteN num)  = Tuple ("/_delete/" <> show num) emptyParams
  link DeleteZ        = Tuple "/_delete" emptyParams



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
