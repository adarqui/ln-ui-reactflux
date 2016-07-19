{-# LANGUAGE OverloadedStrings #-}

module LN.UI.Router.Class.Params2 (
  Params (..),
  emptyParams,
  lookupParam,
  fixParams,
  buildParams,
  fromWebRoutesParams
) where



import           Data.Map    (Map)
import qualified Data.Map    as Map
import           Data.Maybe  (Maybe (..), catMaybes, maybe)
import           Data.Monoid ((<>))
import           Data.Text   (Text)
import qualified Data.Text   as Text
import           Prelude     (String, id, map, read, show, ($), (.))
import           Text.Read   (readMaybe)

import           LN.T        (OrderBy (..), Param (..), ParamTag (..),
                              SortOrderBy (..))
import           LN.UI.Types (Tuple, tuple)




type Params = Map ParamTag Param



emptyParams :: Params
emptyParams = Map.empty



lookupParam :: ParamTag -> Params -> Maybe Param
lookupParam p_tag params = Map.lookup p_tag params



buildParams :: [(ParamTag, Param)] -> Params
buildParams = Map.fromList



sanitizeWebRoutesParams :: [(Text, Maybe Text)] -> [(Text, Text)]
sanitizeWebRoutesParams m_params = catMaybes $ map mapParam m_params
  where
  mapParam (_, Nothing) = Nothing
  mapParam (k, Just v)  = Just (k, v)



fromWebRoutesParams :: [(Text, Maybe Text)] -> [(ParamTag, Param)]
fromWebRoutesParams = catMaybes . map paramFromKV'' . sanitizeWebRoutesParams



fixParams :: Params -> Params
fixParams = id
-- fixParams :: Params -> PSRoutingParams
-- fixParams params = M.fromList $ map (qp <<< snd) $ M.toList params --  M.fromList <<< arrayToList
-- fixParams params = M.fromList $ map (qp <<< snd) $ M.toList params --  M.fromList <<< arrayToList



paramFromKV :: String -> String -> Maybe Param
paramFromKV k v = Nothing



paramFromKV' :: String -> String -> Maybe (Tuple String Param)
paramFromKV' k v =
  case (read k) of
    Nothing    -> Nothing
    Just ParamTag_Limit     -> maybe Nothing (\v -> Just $ tuple k (Limit v)) (read v)
    Just ParamTag_Offset    -> maybe Nothing (\v -> Just $ tuple k (Offset v)) (read v)
    Just ParamTag_Order     -> Just $ tuple k (Order $ read v)
    Just ParamTag_SortOrder -> Just $ tuple k (SortOrder $ read v)
    Just _                  -> Nothing



paramFromKV'' :: (Text, Text) -> Maybe (ParamTag, Param)
paramFromKV'' (k, v) =
  case (readMaybe $ Text.unpack k) of
    Nothing    -> Nothing
    Just ParamTag_Limit     -> maybe Nothing (\v' -> Just (ParamTag_Limit, Limit v')) (readMaybe $ Text.unpack v)
    Just ParamTag_Offset    -> maybe Nothing (\v' -> Just (ParamTag_Offset, Offset v')) (readMaybe $ Text.unpack v)
    Just ParamTag_Order     -> Just (ParamTag_Order, Order $ read $ Text.unpack v)
    Just ParamTag_SortOrder -> Just (ParamTag_SortOrder, SortOrder $ read $ Text.unpack v)
    Just _                  -> Nothing
