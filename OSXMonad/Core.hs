{-# LANGUAGE FlexibleContexts, ForeignFunctionInterface #-}
module OSXMonad.Core where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad
import Data.Char
import Data.List
import System.Exit
import System.IO

import Foreign
import Foreign.C

import Graphics.X11 (Rectangle(..))
import qualified XMonad as XM
import qualified XMonad.Core as C
import qualified XMonad.Layout as L
import qualified XMonad.StackSet as S

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

tile' l r@(Rectangle _ _ screenWidth screenHeight) context = do
  count <- getWindows context
  filledContext <- peek context

  windowsPtrs <- peekArray count . elements $ filledContext
  windows <- mapM (\winPtr -> peek winPtr) windowsPtrs

  namedWindows <- filterM (\win -> (peekCString . name $ win) >>= return . not . all isSpace) windows

  let windowStack = S.Stack 0 [] [fromIntegral x | x <- [1..length namedWindows - 1]]
  let rectangles = C.pureLayout l r windowStack

  let windows' = map (\(i, r) -> rectangleWindow r (namedWindows !! fromIntegral i)) rectangles

  if null namedWindows
     then return ()
     else mapM_ updateWindow windows'

--tile :: C.Layout XM.Window -> Rectangle -> IO ()
tile layout rectangle = do
  transitioning <- isSpaceTransitioning
  if transitioning
    then return ()
    else do
      bracket
         (new (Windows nullPtr))
         (\context -> do
            freeWindows context
            free context)
         (tile' layout rectangle)

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
  hasAPI <- axAPIEnabled
  if not hasAPI
    then do
      hPutStrLn stderr "You need to enable access for Accessible Devices in Universal Access"
      exitWith $ ExitFailure 1
    else do
      s <- screen
      forever $ do
           tile (C.layoutHook initxmc) s
           threadDelay $ 1000 * 500
