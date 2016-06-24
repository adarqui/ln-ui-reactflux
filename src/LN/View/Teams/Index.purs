module LN.View.Teams.Index (
  renderView_Teams_Index,
  renderView_Teams_Index'
) where



import LN.ArrayList                    (listToArray)
import Data.Map                        as M
import Data.Maybe                      (Maybe(..), maybe)
import Halogen                         (ComponentHTML)
import Halogen.HTML.Indexed            as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.Themes.Bootstrap3       as B
import Optic.Core                      ((^.), (..))
import Prelude                         (id, map, show, ($), (<>), (/=))

import LN.Access                       (permissionsHTML', unitDiv)
import LN.Input.Types                  (Input)
import LN.Router.Link                  (linkToP_Classes, linkToP_Glyph', linkToP)
import LN.Router.Types                 (Routes(..), CRUD(..))
import LN.Router.Class.Params          (emptyParams)
import LN.State.Types                  (State)
import LN.View.Helpers
import LN.View.Module.Loading          (renderLoading)
import LN.T                            ( TeamPackResponse
                                       , _TeamPackResponse, _TeamResponse, organization_
                                       , _TeamPackResponse, _TeamResponse, forum_
                                       , OrganizationPackResponse, OrganizationResponse
                                       , _OrganizationPackResponse, _OrganizationResponse
                                       , organization_, team_)



renderView_Teams_Index :: State -> ComponentHTML Input
renderView_Teams_Index st =

  case st.currentOrganization of
       Just org_pack -> renderView_Teams_Index' org_pack st.teams
       _             -> renderLoading



renderView_Teams_Index' :: OrganizationPackResponse -> M.Map Int TeamPackResponse -> ComponentHTML Input
renderView_Teams_Index' org_pack team_packs =
  H.div [P.class_ B.pageHeader] [

    H.h1 [P.class_ B.textCenter] [ H.text "Teams" ],

    H.div [P.class_ B.listUnstyled] $
      map (\team_pack ->
        let team = team_pack ^. _TeamPackResponse .. team_ ^. _TeamResponse in
        H.li_ [
        ]
      )

  ]
