module LN.View.Module.Like (
  renderLike
) where



import Data.Maybe                         (Maybe(..), maybe)
import Halogen                            (ComponentHTML)
import Halogen.HTML.Indexed               as H
import Halogen.HTML.Properties.Indexed    as P
import Halogen.HTML.Events.Indexed        as E
import Halogen.Themes.Bootstrap3          as B
import Halogen.HTML.CSS.Indexed           as HCSS
import CSS                                as CSS
import Optic.Core                         ((^.), (..))
import Prelude                            (($), (<<<))

import LN.Input.Like                      (InputLike(..))
import LN.Input.Star                      (InputStar(..))
import LN.Input.Types                     (Input(..))
import LN.View.Helpers                    ( glyphButton_ArrowUp
                                          , glyphButton_ArrowDown
                                          , glyphButton_Minus
                                          , glyphButton_Star
                                          , glyphButton_StarEmpty
                                          )
import LN.T                               ( Ent
                                          , LikeOpt(..)
                                          , LikeResponse, _LikeResponse
                                          , StarResponse, _StarResponse
                                          , opt_
                                          )



renderLike :: Ent -> Int -> Maybe LikeResponse -> Maybe StarResponse -> ComponentHTML Input
renderLike ent ent_id m_like m_star =

  H.div [P.class_ B.row] [

    glyphButton_ArrowUp colorLike "like" $ CompLike (InputLike_Like ent ent_id m_like),
    glyphButton_Minus colorNeutral "neutral" $ CompLike (InputLike_Neutral ent ent_id m_like),
    glyphButton_ArrowDown colorDislike "dislike" $ CompLike (InputLike_Dislike ent ent_id m_like),

    (case m_star of
         Nothing   -> glyphButton_StarEmpty colorStar "star"
         Just star -> glyphButton_Star colorStar "unstar") $ CompStar (InputStar ent ent_id m_star)
  ]

  where
  color c   = HCSS.style $ CSS.color c
  colorLike =
    case m_like of
         Nothing -> color CSS.black
         Just r  -> case (r ^. _LikeResponse .. opt_) of
                         Like -> color CSS.green
                         _    -> color CSS.black
  colorNeutral =
    case m_like of
         Nothing -> color CSS.black
         Just r  -> case (r ^. _LikeResponse .. opt_) of
                         Neutral -> color CSS.yellow
                         _       -> color CSS.black
  colorDislike =
    case m_like of
         Nothing -> color CSS.black
         Just r  -> case (r ^. _LikeResponse .. opt_) of
                         Dislike -> color CSS.red
                         _       -> color CSS.black
  colorStar   =
    case m_star of
         Nothing -> color CSS.black
         Just _  -> color CSS.orange
  star        =
    case m_star of
         Nothing -> B.glyphiconStarEmpty
         Just _  -> B.glyphiconStar
