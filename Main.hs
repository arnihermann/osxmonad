{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Control.Monad
import Data.Char

import Foreign
import Foreign.C

import Window

main :: IO ()
main = do
  context <- new (Windows nullPtr)

  count <- getWindows context
  filledContext <- peek $ context

  windows <- peekArray count . elements $ filledContext
  mapM_ (\winPtr -> peek winPtr >>= print) windows

  freeWindows context
  free context
