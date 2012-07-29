{-# LANGUAGE FlexibleContexts, ForeignFunctionInterface #-}
module OSXMonad.Core where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad
import Data.Char
import Data.Maybe (fromMaybe)
import Data.List
import System.Exit
import System.IO

import qualified Data.Map as Map

import Foreign
import Foreign.C

import Graphics.X11 (Rectangle(..))
import qualified XMonad as XM
import qualified XMonad.Core as C
import qualified XMonad.Layout as L
import qualified XMonad.StackSet as S

import OSXMonad.Keys
import OSXMonad.Window

updateWindow :: Window -> IO ()
updateWindow window = do
  with window setWindow

rectangleWindow :: Rectangle -> Window -> Window
rectangleWindow r w =
    w { pos = pos', size = size' }
      where
        pos'  = CGPoint {
                  x = fromIntegral $ rect_x r,
                  y = fromIntegral $ rect_y r
                }
        size' = CGSize {
                  width  = fromIntegral $ rect_width  r,
                  height = fromIntegral $ rect_height r
                }

eventModBits :: Event -> XM.ButtonMask
eventModBits event =
    foldl ((. bits) . (+)) 0 [
     (altKey, XM.mod1Mask),
     (commandKey, XM.mod4Mask),
     (controlKey, XM.controlMask),
     (shiftKey, XM.shiftMask)
    ]
        where bits (k, d) = if toBool (k event) then fromIntegral d else 0

getEvent :: IO Event
getEvent = do
  collectEvent
  peek globalEvent

getNamedWindows :: Ptr Windows -> IO [Window]
getNamedWindows context = do
  count <- getWindows context
  filledContext <- peek context

  windowsPtrs <- peekArray count . elements $ filledContext
  windows <- mapM (\winPtr -> peek winPtr) windowsPtrs

  filterM (\win -> (peekCString . name $ win) >>= return . not . all isSpace) windows

tile' :: Rectangle -> Ptr Windows -> XM.X ()
tile' r context = do
  event <- XM.io getEvent

  xmc <- XM.asks XM.config
  ks <- XM.asks XM.keyActions

  let modBits = eventModBits event
      osxKey = osxKeyToX11 . fromIntegral . keyCode $ event
      maybeAction = Map.lookup (modBits, osxKey) ks
  fromMaybe (return ()) maybeAction

  namedWindows <- XM.io . getNamedWindows $ context

  let windowStack = S.Stack 0 [] [fromIntegral x | x <- [1..length namedWindows - 1]]

  (rectangles, _) <- C.runLayout (S.Workspace "OS X" (C.layoutHook xmc) (Just windowStack)) r

  let windows' = map (\(i, r) -> rectangleWindow r (namedWindows !! fromIntegral i)) rectangles

  if null namedWindows
     then return ()
     else XM.io $ mapM_ updateWindow windows'

tile :: Rectangle -> XM.X ()
tile rectangle = do
  transitioning <- XM.io $ isSpaceTransitioning
  if transitioning
    then return ()
    else do
      context <- XM.io . new $ Windows nullPtr
      tile' rectangle context
      XM.io . freeWindows $ context
      XM.io . free $ context

screen :: IO Rectangle
screen = do
  screenPosPtr <- new (CGPoint 0 0)
  screenSizePtr <- new (CGSize 0 0)
  getFrame screenPosPtr screenSizePtr
  screenSize <- peek screenSizePtr
  screenPos <- peek screenPosPtr
  free screenSizePtr
  free screenPosPtr

  return $ Rectangle {
               rect_x = round . x $ screenPos,
               rect_y = round . y $ screenPos,
               rect_width = round . width $ screenSize,
               rect_height = round . height $ screenSize
             }

osxmonad :: (C.LayoutClass l XM.Window, Read (l XM.Window)) => XM.XConfig l -> IO ()
osxmonad initxmc = do
  setupEventCallback

  let display = error "display"
      xmc = initxmc { C.layoutHook = C.Layout $ C.layoutHook initxmc }
      theRoot = 0
      normalBorder = 0
      focusedBorder = 0
      buttonActions = Map.empty
      mouseFocused = False
      mousePosition = Nothing

      windowset = error "windowset"
      mapped = error "mapped"
      waitingUnmap = error "waitingUnmap"
      dragging = error "dragging"
      numberlockMask = error "numberlockMask"
      extensibleState = error "extensibleState"

      conf = C.XConf display xmc theRoot normalBorder focusedBorder (XM.keys xmc xmc) buttonActions mouseFocused mousePosition
      state = C.XState windowset mapped waitingUnmap dragging numberlockMask extensibleState

  hasAPI <- axAPIEnabled
  if not hasAPI
    then do
      hPutStrLn stderr "You need to enable access for Accessible Devices in Universal Access"
      exitWith $ ExitFailure 1
    else do
      s <- screen
      XM.runX conf state . forever $ do
           tile s
           XM.io . threadDelay $ 1000 * 500
      return ()
