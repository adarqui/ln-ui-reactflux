module LN.State.Loading.Helpers (
  getLoading,
  setLoading,
  clearLoading
) where



import LN.State.Types   (State)
import LN.State.Loading as Loading



getLoading :: Int -> State -> Boolean
getLoading key st = Loading.getLoading key st.loading



setLoading :: Int -> State -> State
setLoading key st = st{loading=Loading.setLoading key st.loading}



clearLoading :: Int -> State -> State
clearLoading key st = st{loading=Loading.clearLoading key st.loading}
