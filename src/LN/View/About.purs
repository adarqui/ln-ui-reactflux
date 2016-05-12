module LN.View.About (
  renderView_About
) where



import Halogen              (ComponentHTML)
import Halogen.HTML.Indexed as H

import LN.Input.Types       (Input)



renderView_About :: ComponentHTML Input
renderView_About =
  H.div_
    [ H.h1_ [ H.text "LN" ]
    , H.p_ [ H.text aboutLN ]
    ]



aboutLN :: String
aboutLN = """
Welcome to LN!
"""
