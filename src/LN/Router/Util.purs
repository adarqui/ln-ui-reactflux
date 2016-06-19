module LN.Router.Util (
  mkUri,
  unUri,
  slash,
  preSlash,
  postSlash,
  unslash
) where



import Data.Map               as M
import Data.String            (joinWith, split)
import Data.Tuple             (Tuple)
import Global                 (encodeURI, decodeURI)
import Prelude                ((<<<), (<>))



mkUri :: String -> String
mkUri url = encodeURI url



unUri :: String -> String
unUri url = decodeURI url



-- HACK TODO FIXME: adding trailing slashes (<> /) to all of the Show routes...
-- otherwise we get double ajax calls when we click a bread crumb.. why????????
-- also need slash to make sure there's not multiple trailing slashes, ie, dropWhileEnd which doesn't exist
slash :: String -> String
slash s = s <> "/"



preSlash :: String -> String
preSlash s = "/" <> s



postSlash :: String -> String
postSlash = slash


unslash :: String -> String
unslash = joinWith "" <<< split "/"
