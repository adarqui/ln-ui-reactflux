module LN.Static (
  staticPrefix,
  staticImgPrefix,
  staticGpbpArrowUp,
  staticGpbpArrowUpLit,
  staticGpbpArrowDown,
  staticGpbpArrowDownLit,
  s_me,
  s_users,
  s_organizations,
  s_teams,
  s_forums,
  s_boards,
  s_threads,
  s_threadPosts,
  s_resources,
  s_leurons
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



s_me :: String
s_me = "me"



s_users :: String
s_users = "users"



s_organizations :: String
s_organizations = "organizations"



s_teams :: String
s_teams = "teams"



s_forums :: String
s_forums = "forums"



s_boards :: String
s_boards = "boards"



s_threads :: String
s_threads = "threads"



s_threadPosts :: String
s_threadPosts = "threadPosts"



s_resources :: String
s_resources = "resources"



s_leurons :: String
s_leurons = "leurons"
