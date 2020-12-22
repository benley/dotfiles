{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module XMonad.Hooks.StateHook where

import Control.Monad
import Data.Monoid
import Data.Maybe
import qualified Data.List as L

import XMonad
import XMonad.Util.XUtils(fi)
import XMonad.Util.WindowProperties(getProp32s)

-- | Add more supported states
addSupportedStates :: X ()
addSupportedStates = withDisplay $ \dpy -> do
  r <- asks theRoot
  s <- getAtom "_NET_SUPPORTED"
  supp <- mapM getAtom [ "_NET_WM_STATE"
                       , "_NET_WM_STATE_SKIP_TASKBAR"
                       , "_NET_WM_STATE_FULLSCREEN"
                       , "_NET_WM_STATE_DEMANDS_ATTENTION" ]
  io $ changeProperty32 dpy r s aTOM propModeAppend (map fi supp)

-- | Handle _NET_WM_STATE 'ClientMessageEvent's
stateEventHook :: Event -> X All
stateEventHook ClientMessageEvent { ev_window = win
                                  , ev_event_display = dpy
                                  , ev_message_type = mt
                                  , ev_data = action:state1:state2:_ } = do
  nws <- getAtom "_NET_WM_STATE"
  when (mt == nws) $ do
    state <- fromMaybe [] <$> getProp32s "_NET_WM_STATE" win
    statefs <- fi <$> getAtom "_NET_WM_STATE_FULLSCREEN" -- already handled in fullscreenEventHook
    let estate = filter (/= statefs) . filter (/= 0) . map fi $ [state1, state2]
    case action of
      0 -> do -- _NET_WM_STATE_REMOVE
        let nstate = state L.\\ estate
        io $ changeProperty32 dpy win nws aTOM propModeReplace nstate
      1 -> do -- _NET_WM_STATE_ADD
        let nstate = estate `L.union` state
        io $ changeProperty32 dpy win nws aTOM propModeReplace nstate
      2 -> do -- _NET_WM_STATE_TOGGLE
        let st_add = estate  L.\\         state
            st_rem = estate `L.intersect` state
            nstate = state L.\\ st_rem `L.union` st_add
        io $ changeProperty32 dpy win nws aTOM propModeReplace nstate
      _ -> return ()
  return (All True)
stateEventHook _ = return (All True)

withStateHandle conf = conf { startupHook = startupHook conf >> addSupportedStates
                            , handleEventHook = stateEventHook <+> handleEventHook conf
                            }
