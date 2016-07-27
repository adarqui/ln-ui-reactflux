{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExplicitForAll #-}

module LN.UI.Types (
  Array,
  List,
  Number,
  Tuple,
  tuple,
  String,
  Int,
  pattern Tuple,
  pattern Cons,
  pattern Nil,
  HTMLView,
  HTMLView_,
  HTMLEvent,
  HTMLEvent_,
  OrganizationName,
  ForumName,
  BoardName,
  ThreadName,
  ThreadPostName,
  OrganizationId,
  ForumId,
  BoardId,
  ThreadId,
  ThreadPostId,
  limitInt
) where



import           Data.Int   (Int64)
import           Data.Text  (Text)
import           Prelude
import           React.Flux

import           LN.T.Param



type Array a   = [a]
type List a    = [a]
type Number    = Double
type Tuple a b = (a, b)



tuple :: a -> b -> (a, b)
tuple = (,)


pattern Tuple a b = (a, b)
pattern Cons a a' = (:) a a'
pattern Nil       = []



-- type HTML = ReactElementM EventHandler
-- type HTML' = HTML ()

type HTMLView  = ReactElementM ViewEventHandler
type HTMLView_ = ReactElementM ViewEventHandler ()



type HTMLEvent = forall eventHandler. ReactElementM eventHandler
type HTMLEvent_ = forall eventHandler. ReactElementM eventHandler ()



type OrganizationName = Text
type ForumName        = Text
type BoardName        = Text
type ThreadName       = Text
type ThreadPostName   = Text



type OrganizationId = Int64
type ForumId        = Int64
type BoardId        = Int64
type ThreadId       = Int64
type ThreadPostId   = Int64



limitInt :: Int -> Param
limitInt = Limit . fromIntegral
