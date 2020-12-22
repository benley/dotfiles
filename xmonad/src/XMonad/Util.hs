{-# LANGUAGE LambdaCase #-}
module XMonad.Util where

import Control.Monad
import qualified Data.Map as M

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Hooks.ManageHelpers
import XMonad.Util.WindowProperties

role :: Query String
role = stringProperty "WM_WINDOW_ROLE"

discrete' :: Double -> Double -> Double
discrete' part x
  | x < part     = 0
  | x > 1 - part = 1
  | otherwise    = 0.5

isSplash :: Query Bool
isSplash = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH"

motifNoDecorations :: Query Bool
motifNoDecorations = ask >>= \win -> liftX $ do
  maybemwh <- getProp32s "_MOTIF_WM_HINTS" win
  return $ maybe False (\mwh -> length mwh == 5 && mwh!!2 == 0) maybemwh


infixr 0 ==>
(==>) :: a -> b -> (a, b)
(==>) = (,)

isSticky :: Query Bool
isSticky = isInProperty "_NET_WM_STATE" "_NET_WM_STATE_STICKY"

addKeys :: Ord k => (c -> M.Map k a) -> (c -> M.Map k a) -> c -> M.Map k a
addKeys = liftM2 M.union

runOnWorkspace :: Int -> (WorkspaceId -> X()) -> X()
runOnWorkspace i action = action =<< asks ((!! (i - 1)) . workspaces . config)

runOnScreen :: ScreenId -> (WorkspaceId -> X()) -> X()
runOnScreen screen action = flip whenJust action =<< screenWorkspace screen

swapNth :: Int -> X ()
swapNth = windows . W.modify' . swapNth'

doSwapNth :: Int -> ManageHook
doSwapNth = doF . W.modify' . swapNth'

swapNth' :: Int -> W.Stack a -> W.Stack a
swapNth' n s@(W.Stack c l r)
  | (n < 0) || (n > length l + length r) || (n == length l) = s
  | n < length l = let (nl, nc:nr) = splitAt (length l - n - 1) l in W.Stack nc (nl ++ c : nr) r
  | otherwise    = let (nl, nc:nr) = splitAt (n - length l - 1) r in W.Stack nc l (nl ++ c : nr)

killIfCopy :: X()
killIfCopy = wsContainingCopies >>= \case
  [] -> return ()
  _  -> kill1

toggleOrView' :: WorkspaceId -> X ()
toggleOrView' = toggleOrDoSkip ["NSP"] W.view
