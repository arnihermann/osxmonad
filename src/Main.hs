{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Control.Concurrent
import Control.Monad
import Data.Char
import System.Exit
import System.IO

import Foreign
import Foreign.C

import Window

updateWindow :: Window -> IO ()
updateWindow window = do
  with window setWindow

tileWideWindow :: CDouble -> CDouble -> Int -> Window -> Window
tileWideWindow width height index window =
    window { pos = pos', size = size' }
        where
          pos' = (pos window) { x = fromIntegral index * width, y = 22 } -- Hack: menu bar
          size' = (size window) { width = width, height = height }

tile :: (CGPoint, CGSize) -> IO ()
tile (_, CGSize screenWidth screenHeight) = do
  transitioning <- isSpaceTransitioning
  if transitioning
    then return ()
    else do
      context <- new (Windows nullPtr)

      count <- getWindows context
      filledContext <- peek context

      windowsPtrs <- peekArray count . elements $ filledContext
      windows <- mapM (\winPtr -> peek winPtr) windowsPtrs

      namedWindows <- filterM (\win -> (peekCString . name $ win) >>= return . not . all isSpace) windows

      let tileWidth = screenWidth / fromIntegral (length namedWindows)

      let windows' = map (uncurry $ tileWideWindow tileWidth screenHeight) (zip [0..] namedWindows)

      mapM_ updateWindow windows'

      freeWindows context
      free context

screen :: IO (CGPoint, CGSize)
screen = do
  screenPosPtr <- new (CGPoint 0 0)
  screenSizePtr <- new (CGSize 0 0)
  getFrame screenPosPtr screenSizePtr
  screenSize <- peek screenSizePtr
  screenPos <- peek screenPosPtr
  free screenSizePtr
  free screenPosPtr

  return (screenPos, screenSize)

main :: IO ()
main = do
  hasAPI <- axAPIEnabled
  if not hasAPI
    then do
      hPutStrLn stderr "You need to enable access for Accessible Devices in Universal Access"
      exitWith $ ExitFailure 1
    else do
      s <- screen
      forever $ do
           tile s
           threadDelay $ 1000 * 500
