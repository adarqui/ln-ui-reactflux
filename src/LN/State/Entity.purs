module LN.State.Entity (
  Entity,
  defaultEntity
) where



import Data.Maybe (Maybe(..))
import Data.Date.Helpers (Date, defaultDate)
import LN.Router.Types (Routes(..))



type Entity =
  { nick        :: String
  , displayNick :: String
  , createdAt   :: Maybe Date
  , logo        :: String
  , route       :: Routes
  }



defaultEntity :: Entity
defaultEntity =
  { nick:        "unknown"
  , displayNick: "unknown"
  , createdAt:   Just defaultDate
  , logo:        ""
  , route:       NotFound
}
