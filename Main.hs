{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Control.Monad
import Data.Char

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
          pos' = (pos window) { x = fromIntegral index * width, y = 0 }
          size' = (size window) { width = width, height = height }

tileWide :: IO ()
tileWide = do
  screenSizePtr <- new (CGSize 0 0)
  getScreenSize screenSizePtr
  screenSize <- peek screenSizePtr
  free screenSizePtr

  context <- new (Windows nullPtr)

  count <- getWindows context
  filledContext <- peek context

  windowsPtrs <- peekArray count . elements $ filledContext
  windows <- mapM (\winPtr -> peek winPtr) windowsPtrs

  let tileWidth = width screenSize / fromIntegral count
  let screenHeight = height screenSize

  let windows' = map (uncurry $ tileWideWindow tileWidth screenHeight) (zip [0..] windows)

  mapM_ updateWindow windows'

  freeWindows context
  free context

main :: IO ()
main = do
  forever tileWide
