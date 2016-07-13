module LN.UI.Router.Class.CRUD (
  CRUD (..),
  TyCRUD (..)
) where



import qualified Data.Map                  as M
import           Data.Monoid               ((<>))
import           Data.Tuple                (fst)
import           Prelude                   (Bool (..), Eq, Show, show, ($),
                                            (++), (==))

import           LN.UI.Router.Class.Link   (HasLink (..))
import           LN.UI.Router.Class.Params (emptyParams)
import           LN.UI.Types               (Int, Number, String, tuple)



data CRUD
  = Index
  | Show  String
  | ShowI Int
  | ShowN Number
  | ShowB Bool
  | New
  | Edit String
  | EditI Int
  | EditN Number
  | Delete String
  | DeleteI Int
  | DeleteN Number
  | DeleteZ



instance Eq CRUD where
  (==) Index        Index        = True
  (==) (Show t1)    (Show t2)    = t1 == t2
  (==) (ShowI t1)   (ShowI t2)   = t1 == t2
  (==) (ShowN t1)   (ShowN t2)   = t1 == t2
  (==) (ShowB t1)   (ShowB t2)   = t1 == t2
  (==) New          New          = True
  (==) (Edit t1)    (Edit t2)    = t1 == t2
  (==) (EditI t1)   (EditI t2)   = t1 == t2
  (==) (EditN t1)   (EditN t2)   = t1 == t2
  (==) (Delete t1)  (Delete t2)  = t1 == t2
  (==) (DeleteI t1) (DeleteI t2) = t1 == t2
  (==) (DeleteN t1) (DeleteN t2) = t1 == t2
  (==) DeleteZ      DeleteZ      = True
  (==) _            _            = False



instance Show CRUD where
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



instance HasLink CRUD where
-- TODO FIXME:
-- well this could be fixed.. changed from "" in order to match CRUD Index routes
--  link Index    = Tuple "/index" emptyParams
  link Index          = tuple "" emptyParams
  link New            = tuple "/new" emptyParams
  link (Show s)       = tuple ("/" <> s) emptyParams
  link (ShowI int)    = tuple ("/" <> show int) emptyParams
  link (ShowN num)    = tuple ("/" <> show num) emptyParams
  link (ShowB bool)   = tuple ("/" <> show bool) emptyParams
  link (Edit s)       = tuple ("/_edit/" <> s) emptyParams
  link (EditI int)    = tuple ("/_edit/" <> show int) emptyParams
  link (EditN num)    = tuple ("/_edit/" <> show num) emptyParams
  link (Delete s)     = tuple ("/_delete/" <> s) emptyParams
  link (DeleteI int)  = tuple ("/_delete/" <> show int) emptyParams
  link (DeleteN num)  = tuple ("/_delete/" <> show num) emptyParams
  link DeleteZ        = tuple "/_delete" emptyParams



data TyCRUD
  = TyCreate
  | TyEdit
  | TyDelete
  | TyView



instance Eq TyCRUD where
  (==) TyCreate TyCreate = True
  (==) TyEdit   TyEdit   = True
  (==) TyDelete TyDelete = True
  (==) TyView   TyView   = True
  (==) _        _        = False



instance Show TyCRUD where
  show TyCreate = "Create"
  show TyEdit   = "Edit"
  show TyDelete = "Delete"
  show TyView   = "View"
