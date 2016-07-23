{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module LN.UI.Router.CRUD (
  CRUD (..),
  TyCRUD (..)
) where



import           Control.Applicative
import           Control.DeepSeq                    (NFData)
import qualified Data.Map                           as M
import           Data.Monoid                        ((<>))
import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import           Data.Tuple                         (fst)
import           Prelude                            (Bool (..), Eq, Show, pure,
                                                     show, ($), (++), (==))
import           Text.ParserCombinators.Parsec.Prim (try)
import           Web.Routes

import           LN.UI.Router.Link            (HasLink (..))
import           LN.UI.Router.Param          (emptyParams)
import           LN.UI.Types                        (Int, Number, String, tuple)



data CRUD
  = Index
  | ShowS Text
  | ShowI Int
--  | ShowN Number
  | ShowB Bool
  | New
  | EditS Text
  | EditI Int
--  | EditN Number
  | DeleteS Text
  | DeleteI Int
--  | DeleteN Number
  | DeleteZ
  deriving (Eq, Show, Generic, NFData)



instance HasLink CRUD where
-- TODO FIXME:
-- well this could be fixed.. changed from "" in order to match CRUD Index routes
  -- link Index          = tuple "" emptyParams
  -- link New            = tuple "/new" emptyParams
  -- link (Show s)       = tuple ("/" <> s) emptyParams
  -- link (ShowI int)    = tuple ("/" <> show int) emptyParams
  -- link (ShowN num)    = tuple ("/" <> show num) emptyParams
  -- link (ShowB bool)   = tuple ("/" <> show bool) emptyParams
  -- link (Edit s)       = tuple ("/_edit/" <> s) emptyParams
  -- link (EditI int)    = tuple ("/_edit/" <> show int) emptyParams
  -- link (EditN num)    = tuple ("/_edit/" <> show num) emptyParams
  -- link (Delete s)     = tuple ("/_delete/" <> s) emptyParams
  -- link (DeleteI int)  = tuple ("/_delete/" <> show int) emptyParams
  -- link (DeleteN num)  = tuple ("/_delete/" <> show num) emptyParams
  -- link DeleteZ        = tuple "/_delete" emptyParams

instance PathInfo CRUD where
  toPathSegments crud =
    case crud of
      Index     -> [""]
      New       -> ["new"]
      ShowS s   -> [s]
      ShowI i   -> [Text.pack $ show i]
      ShowB b   -> [bool2Text b]
      EditS s   -> ["_edit", s]
      EditI i   -> ["_edit", Text.pack $ show i]
      DeleteS s -> ["_delete", s]
      DeleteI i -> ["_delete", Text.pack $ show i]
      DeleteZ   -> ["_delete"]
  fromPathSegments =
        New     <$  segment "new"

    -- TODO FIXME: This is hideous.
    <|>      (try (EditI <$  segment "_edit"   <*> fromPathSegments)
         <|> EditS <$  segment "_edit"   <*> fromPathSegments)

    -- TODO FIXME: This is hideous.
    <|>      (try (DeleteI <$  segment "_delete" <*> fromPathSegments)
         <|> (try (DeleteS <$  segment "_delete" <*> fromPathSegments)
         <|> DeleteZ <$  segment "_delete"))

    <|> ShowI   <$> fromPathSegments
    <|> ShowB   <$> fromPathSegments
    <|> ShowS   <$> fromPathSegments
    <|> pure Index
    -- TODO FIXME: Can't do Index <$ segment "" because it fails to pattern match. Though, pure Index works because it's terminal.


bool2Text :: Bool -> Text
bool2Text True  = "true"
bool2Text False = "false"

text2Bool :: Text -> Bool
text2Bool "true" = True
text2Bool _      = False



instance PathInfo Bool where
  toPathSegments bool =
    case bool of
      True  -> ["true"]
      False -> ["false"]
  fromPathSegments =
        True <$ segment "true"
    <|> False <$ segment "false"



  -- link (ShowN num)    = tuple ("/" <> show num) emptyParams
  -- link (ShowB bool)   = tuple ("/" <> show bool) emptyParams
  -- link (Edit s)       = tuple ("/_edit/" <> s) emptyParams
  -- link (EditI int)    = tuple ("/_edit/" <> show int) emptyParams
  -- link (EditN num)    = tuple ("/_edit/" <> show num) emptyParams
  -- link (Delete s)     = tuple ("/_delete/" <> s) emptyParams
  -- link (DeleteI int)  = tuple ("/_delete/" <> show int) emptyParams
  -- link (DeleteN num)  = tuple ("/_delete/" <> show num) emptyParams
  -- link DeleteZ        = tuple "/_delete" emptyParams




data TyCRUD
  = TyCreate
  | TyEdit
  | TyDelete
  | TyView
  deriving (Eq, Generic)



instance Show TyCRUD where
  show TyCreate = "Create"
  show TyEdit   = "Edit"
  show TyDelete = "Delete"
  show TyView   = "View"
