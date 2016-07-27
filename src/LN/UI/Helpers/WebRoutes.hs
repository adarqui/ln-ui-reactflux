{-# LANGUAGE DeriveAnyClass #-}

module LN.UI.Helpers.WebRoutes (
  str1
) where



import           Data.Text        (Text)
import qualified Data.Text        as Text (null, unpack)
import           Text.Parsec.Prim ((<?>))
import           Web.Routes



str1 :: URLParser Text
str1 = (pToken (const ()) (\y -> if (not $ Text.null y) then Just y else Nothing)) <?> "String is empty"
