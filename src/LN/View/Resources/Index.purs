module LN.View.Resources.Index (
  renderView_Resources_Index
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
import LN.Router.Link                  (linkToP_Classes)
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.State.Types                  (State)
import LN.State.User                   (usersMapLookup_ToUser)
import LN.View.Module.Gravatar         (renderGravatarForUser)
import LN.View.Module.Loading          (renderLoading)
import LN.View.Module.OrderBy          (renderOrderBy)
import LN.View.Module.PageNumbers      (renderPageNumbers)
import LN.T                            ( Size(Small)
                                       , _ResourceStatResponse, _ResourcePackResponse, _ResourceResponse
                                       , stat_, resource_)



renderView_Resources_Index :: State -> ComponentHTML Input
renderView_Resources_Index st =
  if M.isEmpty st.resources
     then renderLoading
     else renderView_Resources_Index' st



renderView_Resources_Index' :: State -> ComponentHTML Input
renderView_Resources_Index' st =

  H.div [P.class_ B.containerFluid] [

    H.div [P.class_ B.pageHeader] [
      H.h2_ [H.text "Resources"]
    ],

    H.div [P.classes [B.colLg2, B.colMd2, B.colXs12]] [
      linkToP_Classes [B.btn, B.btnLg, B.btnInfo, B.btnBlock] [] (Resources New []) "new"
    ],

    -- Page Numbers
    H.div [P.class_ B.clearfix] [H.span [P.classes [B.pullLeft]] [renderOrderBy st.currentPage]],

    -- Resources
    H.div [] [resources st]
  ]



resources :: State -> ComponentHTML Input
resources st =
  H.div [P.class_ B.containerFluid] [
    renderPageNumbers st.resourcesPageInfo st.currentPage
    , H.ul [P.class_ B.listUnstyled] $
        map (\pack ->
          let
            resource_pack = pack ^. _ResourcePackResponse
            resource      = pack ^. _ResourcePackResponse .. resource_ ^. _ResourceResponse
            stat          = pack ^. _ResourcePackResponse .. stat_ ^. _ResourceStatResponse
          in
          H.li_ [
            H.div [P.class_ B.row] [
                H.div [P.class_ B.colXs1] [
                  renderGravatarForUser Small (usersMapLookup_ToUser st resource.userId)
                ]
              , H.div [P.class_ B.colXs7] [
                    H.div [P.class_ B.listGroup] [linkToP_Classes [B.listGroupItem] [] (Resources (Show $ show resource.id) []) resource.displayName]
                  , H.p_ [H.text $ show resource.createdAt]
                  , H.p_ [H.text $ resource.description]
                ]
              , H.div [P.class_ B.colXs2] [
                  H.p_ [H.text $ show stat.leurons <> " leurons"]
                ]
              , H.div [P.class_ B.colXs2] [
                  H.p_ [H.text $ show stat.views <> " views"]
                ]
--              , H.div [P.class_ B.colXs3] [
--                  H.div_ [ H.p_ [H.text "Likes?"]]
--              ]
            ]
          ])
        $ listToArray $ M.values st.resources
    , renderPageNumbers st.resourcesPageInfo st.currentPage
  ]
