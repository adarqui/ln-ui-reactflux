module LN.Static (
  staticPrefix,
  staticImgPrefix,
  staticGpbpArrowUp,
  staticGpbpArrowUpLit,
  staticGpbpArrowDown,
  staticGpbpArrowDownLit
) where



import Prelude ((<>))



staticPrefix :: String
staticPrefix = "/static"



staticImgPrefix :: String
staticImgPrefix = staticPrefix <> "/img"



staticGpbpArrowUp :: String
staticGpbpArrowUp = "gpbp_arrow_up.gif"



staticGpbpArrowUpLit :: String
staticGpbpArrowUpLit = "gpbp_arrow_up_lit.gif"



staticGpbpArrowDown :: String
staticGpbpArrowDown = "gpbp_arrow_down.gif"



staticGpbpArrowDownLit :: String
staticGpbpArrowDownLit = "gpbp_arrow_down_lit.gif"
