module LN.View.Portal.Resources (
  renderView_Portal_Resources
) where



import Halogen                         (ComponentHTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3       as B

import LN.Input.Types                  (Input)
import LN.State.Types                  (State)
import LN.View.Module.OrderBy
import LN.View.Module.PageNumbers
import LN.T



renderView_Portal_Resources :: State -> ComponentHTML Input
renderView_Portal_Resources st =
  H.div [P.class_ B.containerFluid] [
    H.div [P.class_ B.pageHeader] [
      H.h2_ [H.text "Resources"]
    ],
    H.div [P.class_ B.clearfix] [H.span [P.classes [B.pullLeft]] [renderOrderBy st.currentPage]],
    H.div [] [resources st]
  ]



resources :: State -> ComponentHTML Input
resources st =
  H.div_ [
    renderPageNumbers st.resourcesPageInfo st.currentPage
  ]
