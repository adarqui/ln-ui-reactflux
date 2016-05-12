module LN.Router.Internal where

import Prelude
import Data.Map as M
import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Routing.Hash.Aff
import DOM
import Purescript.Api.Helpers
import Daimyo.Data.ArrayList
import LN
import LN.Router.Types
import LN.Router.Util



(</>) :: forall a b. (HasLink a, HasLink b) => (a -> b) -> a -> b
(</>) = ($)



updateUrl :: forall e. Routes -> Aff (dom :: DOM | e) Unit
updateUrl route =
  let l = link route
--   in setHash $ drop 1 $ ((fst l) <> (mkQueryString $ flattenParams $ listToArray $  M.toList $ snd l))

   in setHash $ drop 1 $ mkUri $ ((fst l) <> "/" <> (mkQueryString $ flattenParams $ listToArray $  M.toList $ snd l))
--   in setHash $ drop 1 $ ((fst l) ++ (mkQueryString $ flattenParams $ listToArray $  M.toList $ snd l))
-- updateUrl = setHash <<< drop 1 <<< link



linkTo :: Routes -> String -> HTML _ _
linkTo r t =
  let l = link r
   in H.a [ P.href $ mkUri $ (fst l) <> "/" ] [ H.text t ]



linkToHref :: Routes -> P.IProp _ _
linkToHref r =
  let l = link r
   in P.href $ mkUri $ (fst l) <> "/"



linkTo' :: Routes -> Array (HTML _ _) -> HTML _ _
linkTo' r v =
  let l = link r
   in H.a [ P.href $ mkUri $ (fst l) <> "/"] v



linkToP :: Array Param -> Routes -> String -> HTML _ _
linkToP params r t =
  let l = link r
   in H.a [P.href $ mkUri $ (fst l) <> "/" <> (mkQueryString $ flattenParams $ map qp params)] [H.text t]



linkToP_Classes :: Array H.ClassName -> Array Param -> Routes -> String -> HTML _ _
linkToP_Classes classes params r t =
  let l = link r
   in H.a [P.classes classes, P.href $ mkUri $ (fst l) <> "/" <> (mkQueryString $ flattenParams $ map qp params)] [H.text t]



-- so pointless
linkToP_Classes' :: Array Param -> Routes -> String -> HTML _ _
linkToP_Classes' = linkToP_Classes []
