module LN.View.Leurons.Index (
  renderView_Leurons_Index
) where



import Daimyo.Data.ArrayList           (listToArray)
import Data.Map                        as M
import Halogen                         (ComponentHTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3       as B
import Optic.Core                      ((^.), (..))
import Prelude                         (show, map, ($), (<>))

import LN.Input.Types                  (Input)
import LN.Router.Link                  (linkToP)
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.Router.Class.Params          (emptyParams)
import LN.State.Types                  (State)
import LN.State.User                   (usersMapLookup_ToUser)
import LN.View.Module.Gravatar         (renderGravatarForUser)
import LN.View.Module.Loading          (renderLoading)
import LN.View.Module.OrderBy          (renderOrderBy)
import LN.View.Module.PageNumbers      (renderPageNumbers)
import LN.T                            ( Size(Small)
                                       , _LeuronStatResponse, _LeuronPackResponse, _LeuronResponse
                                       , stat_, leuron_)



renderView_Leurons_Index :: State -> ComponentHTML Input
renderView_Leurons_Index st =
  if M.isEmpty st.leurons
     then renderLoading
     else renderView_Leurons_Index' st



renderView_Leurons_Index' :: State -> ComponentHTML Input
renderView_Leurons_Index' st =

  H.div [P.class_ B.containerFluid] [

    H.div [P.class_ B.pageHeader] [
      H.h2_ [H.text "Leurons"]
    ],

    -- Page Numbers
    H.div [P.class_ B.clearfix] [H.span [P.classes [B.pullLeft]] [renderOrderBy st.currentPage]],

    -- Leurons
    H.div [] [renderLeurons st]
  ]



renderLeurons :: State -> ComponentHTML Input
renderLeurons st =
  H.div_ [
    renderPageNumbers st.leuronsPageInfo st.currentPage
    , H.ul [P.class_ B.listUnstyled] $
        map (\pack ->
          let
            leuron_pack = pack ^. _LeuronPackResponse
            leuron      = pack ^. _LeuronPackResponse .. leuron_ ^. _LeuronResponse
            stat        = pack ^. _LeuronPackResponse .. stat_ ^. _LeuronStatResponse
          in
          H.li_ [
            H.div [P.class_ B.row] [
                H.div [P.class_ B.colSm1] [
                  renderGravatarForUser Small (usersMapLookup_ToUser st leuron.userId)
                ]
              , H.div [P.class_ B.colSm6] [
                    linkToP [] (ResourcesLeurons leuron.resourceId (ShowI leuron.id) emptyParams) (show leuron.id)
                  , H.p_ [H.text $ show leuron.createdAt]
--                  , H.p_ [H.text leuron.description]
                ]
              , H.div [P.class_ B.colSm1] [
                  H.p_ [H.text $ show stat.views <> " views"]
                ]
              , H.div [P.class_ B.colSm3] [
                  H.div_ [ H.p_ [H.text "Likes?"]]
              ]
            ]
          ])
        $ listToArray $ M.values st.leurons
    , renderPageNumbers st.leuronsPageInfo st.currentPage
  ]
