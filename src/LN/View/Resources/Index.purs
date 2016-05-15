module LN.View.Resources.Index (
  renderView_Resources_Index
) where



import Daimyo.Data.ArrayList           (listToArray)
import Data.Map                        as M
import Halogen                         (ComponentHTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.Events.Indexed     as E
import Halogen.Themes.Bootstrap3       as B
import Optic.Core                      ((^.), (..))
import Prelude                         (id, show, map, ($), (<>))

import LN.Input.Types                  (Input(..))
import LN.Router.Internal              (linkToP, linkToP_Glyph')
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.State.Types                  (State)
import LN.State.User                   (usersMapLookup_ToUser)
import LN.View.Module.Gravatar
import LN.View.Module.OrderBy
import LN.View.Module.PageNumbers
import LN.T



renderView_Resources_Index :: State -> ComponentHTML Input
renderView_Resources_Index st =

  H.div [P.class_ B.containerFluid] [

    H.div [P.class_ B.pageHeader] [
      H.h2_ [H.text "Resources"]
    ],

    -- Page Numbers
    H.div [P.class_ B.clearfix] [H.span [P.classes [B.pullLeft]] [renderOrderBy st.currentPage]],

    -- Create a new resource
    linkToP_Glyph' (Resources New []) B.glyphiconPlus,

    -- Resources
    H.div [] [resources st]
  ]



resources :: State -> ComponentHTML Input
resources st =
  H.div_ [
    renderPageNumbers st.resourcesPageInfo st.currentPage
    , H.ul [P.class_ B.listUnstyled] $
        map (\(pack@(ResourcePackResponse resource_pack)) ->
          H.li_ [
            H.div [P.class_ B.row] [
                H.div [P.class_ B.colSm1] [
                  renderGravatarForUser Small (usersMapLookup_ToUser st (t pack ^. userId_))
                ]
              , H.div [P.class_ B.colSm6] [
                    linkToP [] (Resources (Show $ show $ t pack ^. id_) []) (t pack ^. title_)
                  , H.p_ [H.text $ show $ t pack ^. createdAt_]
                  , H.p_ [H.text $ t pack ^. description_ ]
                ]
              , H.div [P.class_ B.colSm1] [
                  H.p_ [H.text $ show (ts pack ^. leurons_) <> " leurons"]
                ]
              , H.div [P.class_ B.colSm1] [
                  H.p_ [H.text $ show (ts pack ^. views_) <> " views"]
                ]
              , H.div [P.class_ B.colSm3] [
                  H.div_ [ H.p_ [H.text "Likes?"]]
              ]
            ]
          ])
        $ listToArray $ M.values st.resources
    , renderPageNumbers st.resourcesPageInfo st.currentPage
  ]
  where
  -- resource
  t x = (x ^. _ResourcePackResponse .. resource_ ^. _ResourceResponse)
  -- resource user
  tu x = (x ^. _ResourcePackResponse .. user_ ^. _UserSanitizedResponse)
  -- resource stat
  ts x = (x ^. _ResourcePackResponse .. stat_ ^. _ResourceStatResponse)
  -- resource post
--  tp x = (x ^. _ResourcePackResponse .. latestResource_)
  -- resource post user
--  tpu x = (x ^. _ResourcePackResponse .. latestResourceUser_)
