{-# LANGUAGE FlexibleContexts, ForeignFunctionInterface #-}
module OSXMonad.Core where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad
import Data.Char
import Data.Maybe (fromMaybe)
import Data.List
import System.Exit
import System.IO

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

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

focusWindow :: Window -> IO ()
focusWindow window = do
  with window setWindowFocused

rectangleWindow :: Rectangle -> Window -> Window
rectangleWindow (Rectangle x y w h) win =
    win { pos = pos', size = size' }
      where
        pos'  = CGPoint {
                  x = fromIntegral x,
                  y = fromIntegral y
                }
        size' = CGSize {
                  width = fromIntegral w,
                  height = fromIntegral h
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

tile' :: Ptr Windows -> XM.X ()
tile' context = do
  event <- XM.io getEvent

  ks <- XM.asks XM.keyActions

  let modBits = eventModBits event
      osxKey = osxKeyToX11 . fromIntegral . keyCode $ event
      maybeAction = Map.lookup (modBits, osxKey) ks
  fromMaybe (return ()) maybeAction

  ws <- XM.gets C.windowset

  namedWindows <- XM.io . getNamedWindows $ context
  let wids = map (fromIntegral . wid) namedWindows
      newStack = S.modify Nothing (S.filter (`elem` wids)) $ foldr S.insertUp ws $ wids

  XM.modify (\s -> s { XM.windowset = newStack })

  let rect = C.screenRect . S.screenDetail . S.current $ ws
  (rectangles, _) <- C.runLayout (S.workspace . S.current $ ws) rect

  let namedWindowsById = zip wids namedWindows
      focusedWindow = S.peek newStack >>= flip lookup namedWindowsById
      windows' = Maybe.catMaybes
                 $ map (\(i, r) ->
                        fmap (rectangleWindow r) (lookup i namedWindowsById)
                       ) rectangles

  maybe (return ()) (XM.io . focusWindow) focusedWindow

  if null namedWindows
     then return ()
     else XM.io $ mapM_ updateWindow windows'

tile :: XM.X ()
tile = do
  transitioning <- XM.io $ isSpaceTransitioning
  if transitioning
    then return ()
    else do
      context <- XM.io . new $ Windows nullPtr
      tile' context
      XM.io . freeWindows $ context
      XM.io . free $ context

screenRectangle :: IO Rectangle
screenRectangle = do
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

osxWindows :: (XM.WindowSet -> XM.WindowSet) -> XM.X ()
osxWindows f = do
  XM.XState { C.windowset = old } <- XM.get
  let ws = f old
  XM.modify (\s -> s { C.windowset = ws })

osxSendMessage :: C.Message a => a -> XM.X ()
osxSendMessage a = do
    w <- S.workspace . S.current <$> XM.gets C.windowset
    ml' <- C.handleMessage (S.layout w) (C.SomeMessage a) `C.catchX` return Nothing
    C.whenJust ml' $ \l' ->
        osxWindows $ \ws -> ws { S.current = (S.current ws)
                                { S.workspace = (S.workspace $ S.current ws)
                                  { S.layout = l' }}}

osxKeys :: C.XConfig C.Layout -> Map.Map (XM.KeyMask, XM.KeySym) (C.X ())
osxKeys (C.XConfig {C.modMask = modMask}) = Map.fromList
          [ ((modMask,                  XM.xK_space ), osxSendMessage L.NextLayout)
          , ((modMask,                  XM.xK_Tab   ), osxWindows S.focusDown)
          , ((modMask .|. XM.shiftMask, XM.xK_Tab   ), osxWindows S.focusUp  )
          , ((modMask,                  XM.xK_j     ), osxWindows S.focusDown)
          , ((modMask,                  XM.xK_k     ), osxWindows S.focusUp  )
          , ((modMask,                  XM.xK_m     ), osxWindows S.focusMaster  )
          , ((modMask,                  XM.xK_Return), osxWindows S.swapMaster)
          , ((modMask .|. XM.shiftMask, XM.xK_j     ), osxWindows S.swapDown  )
          , ((modMask .|. XM.shiftMask, XM.xK_k     ), osxWindows S.swapUp    )
          , ((modMask,                  XM.xK_h     ), osxSendMessage L.Shrink)
          , ((modMask,                  XM.xK_l     ), osxSendMessage L.Expand)
          ]

osxmonad :: (C.LayoutClass l XM.Window, Read (l XM.Window)) => XM.XConfig l -> IO ()
osxmonad initxmc = do
  setupEventCallback

  rect <- screenRectangle

  let display = error "display"
      xmc = initxmc { C.layoutHook = C.Layout $ C.layoutHook initxmc }
      theRoot = 0
      normalBorder = 0
      focusedBorder = 0
      buttonActions = Map.empty
      mouseFocused = False
      mousePosition = Nothing

      layout = C.layoutHook xmc

      windowset = S.new layout (C.workspaces xmc) $ [C.SD rect] -- TODO: All screen sizes
      mapped = Set.empty
      waitingUnmap = Map.empty
      dragging = Nothing
      numberlockMask = 0
      extensibleState = Map.empty

      conf = C.XConf display xmc theRoot normalBorder focusedBorder (XM.keys xmc xmc) buttonActions mouseFocused mousePosition
      state = C.XState windowset mapped waitingUnmap dragging numberlockMask extensibleState

  hasAPI <- axAPIEnabled
  if not hasAPI
    then do
      hPutStrLn stderr "You need to enable access for Accessible Devices in Universal Access"
      exitWith $ ExitFailure 1
    else do
      XM.runX conf state . forever $ do
           tile
           XM.io . threadDelay $ 1000 * 500
      return ()
