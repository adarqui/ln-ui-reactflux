module LN.Router.Link (
  (</>),
  apply',
  updateUrl,
  linkTo,
  linkTo',
  linkToP,
  linkToP_Classes,
  linkToP_Classes',
  linkToP_Glyph,
  linkToP_Glyph',
  linkToHref
) where



import Control.Monad.Aff               (Aff)
import Daimyo.Data.ArrayList           (listToArray)
import Data.Map                        as M
import Data.String                     (drop)
import Data.Tuple                      (fst, snd)
import DOM                             (DOM)
import Halogen                         (HTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3       as B
import Routing.Hash.Aff                (setHash)
import Prelude                         (Unit, map, ($), (<>))
import Purescript.Api.Helpers          (mkQueryString, flattenParams, qp)

import LN.Router.Types                 (Routes, class HasLink, link)
import LN.Router.Util                  (mkUri)
import LN.T.Internal.Types             (Param)



infixl 9 apply' as </>

apply' :: forall a b. (HasLink a, HasLink b) => (a -> b) -> a -> b
apply' = ($)



updateUrl :: forall e. Routes -> Aff (dom :: DOM | e) Unit
updateUrl route =
  let l = link route
--   in setHash $ drop 1 $ ((fst l) <> (mkQueryString $ flattenParams $ listToArray $  M.toList $ snd l))

   in setHash $ drop 1 $ mkUri $ ((fst l) <> "/" <> (mkQueryString $ flattenParams $ listToArray $  M.toList $ snd l))
--   in setHash $ drop 1 $ ((fst l) ++ (mkQueryString $ flattenParams $ listToArray $  M.toList $ snd l))
-- updateUrl = setHash <<< drop 1 <<< link



-- | Create a link to a route, providing a string as the anchor name
--
linkTo :: Routes -> String -> HTML _ _
linkTo r t =
  let l = link r
   in H.a [ P.href $ mkUri $ (fst l) <> "/" ] [ H.text t ]



-- | Create a link to a route, but provide an array of html elements instead of an anchor name
--
linkTo' :: Routes -> Array (HTML _ _) -> HTML _ _
linkTo' r v =
  let l = link r
   in H.a [ P.href $ mkUri $ (fst l) <> "/"] v



-- | Create a link with Params
--
linkToP :: Array Param -> Routes -> String -> HTML _ _
linkToP params r t =
  let l = link r
   in H.a [P.href $ mkUri $ (fst l) <> "/" <> (mkQueryString $ flattenParams $ map qp params)] [H.text t]



-- | Create a link with class names as properties
--
linkToP_Classes :: Array H.ClassName -> Array Param -> Routes -> String -> HTML _ _
linkToP_Classes classes params r t =
  let l = link r
   in H.a [P.classes classes, P.href $ mkUri $ (fst l) <> "/" <> (mkQueryString $ flattenParams $ map qp params)] [H.text t]



-- so pointless
--
linkToP_Classes' :: Array Param -> Routes -> String -> HTML _ _
linkToP_Classes' = linkToP_Classes []



-- | Create a link with Params, with a glyphicon
--
linkToP_Glyph :: Array Param -> Routes -> H.ClassName -> HTML _ _
linkToP_Glyph params r glyph =
  H.a [
      P.href $ mkUri $ (fst l) <> "/" <> (mkQueryString $ flattenParams $ map qp params)
    ] [
      H.span [P.classes [B.glyphicon, glyph]] []
    ]
  where
  l = link r



-- | Create a link with a glyphicon
--
linkToP_Glyph' :: Routes -> H.ClassName -> HTML _ _
linkToP_Glyph' = linkToP_Glyph []



-- | Create a link, but simply give us the property
--
linkToHref :: Routes -> P.IProp _ _
linkToHref r =
  let l = link r
   in P.href $ mkUri $ (fst l) <> "/"
