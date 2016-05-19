module LN.Router.Util (
  mkUri,
  unUri,
  fixParams,
  slash,
  preSlash,
  postSlash
) where



import Daimyo.Data.ArrayList (arrayToList)
import Data.Tuple            (Tuple)
import Data.Map              as M
import Global                (encodeURI, decodeURI)
import Prelude               ((<<<), (<>))



mkUri :: String -> String
mkUri url = encodeURI url



unUri :: String -> String
unUri url = decodeURI url



fixParams :: Array (Tuple String String) -> M.Map String String
fixParams = M.fromList <<< arrayToList



-- HACK TODO FIXME: adding trailing slashes (<> /) to all of the Show routes...
-- otherwise we get double ajax calls when we click a bread crumb.. why????????
-- also need slash to make sure there's not multiple trailing slashes, ie, dropWhileEnd which doesn't exist
slash :: String -> String
slash s = s <> "/"



preSlash :: String -> String
preSlash s = "/" <> s



postSlash :: String -> String
postSlash = slash
