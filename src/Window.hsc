{-# LANGUAGE ForeignFunctionInterface #-}
module Window where

import Foreign
import Foreign.C

#include "utils.h"

#if CGFLOAT_IS_DOUBLE
#let float_type = "CDouble"
#else
#let float_type = "CFloat"
#endif

foreign import ccall "ApplicationServices/ApplicationServices.h AXAPIEnabled"
  axAPIEnabled :: IO Bool

foreign import ccall "utils.h setWindow"
  setWindow :: Ptr Window -> IO ()

foreign import ccall "utils.h getWindows"
  getWindows :: Ptr Windows -> IO Int

foreign import ccall "utils.h freeWindows"
  freeWindows :: Ptr Windows -> IO ()

foreign import ccall "utils.h getFrame"
  getFrame :: Ptr CGPoint -> Ptr CGSize -> IO ()

foreign import ccall "utils.h isSpaceTransitioning"
  isSpaceTransitioning :: IO Bool

data CGPoint = CGPoint { x :: (#float_type), y :: (#float_type) } deriving Show
data CGSize = CGSize { width :: (#float_type), height :: (#float_type) } deriving Show

data AXUIElement

data Window = Window {
      uiElement :: Ptr AXUIElement,
      name :: CString,
      pos :: CGPoint,
      size :: CGSize
    } deriving Show

data Windows = Windows { elements :: Ptr (Ptr Window) } deriving Show

instance Storable CGPoint where
    sizeOf _ = (#size CGPoint)
    alignment _ = alignment (undefined :: (#float_type))
    peek ptr = do
      x' <- (#peek CGPoint, x) ptr
      y' <- (#peek CGPoint, y) ptr
      return $ CGPoint x' y'
    poke ptr (CGPoint x' y') = do
        (#poke CGPoint, x) ptr x'
        (#poke CGPoint, y) ptr y'

instance Storable CGSize where
    sizeOf _ = (#size CGSize)
    alignment _ = alignment (undefined :: (#float_type))
    peek ptr = do
      width' <- (#peek CGSize, width) ptr
      height' <- (#peek CGSize, height) ptr
      return $ CGSize width' height'
    poke ptr (CGSize width' height') = do
        (#poke CGSize, width) ptr width'
        (#poke CGSize, height) ptr height'

instance Storable Window where
    sizeOf _ = (#size Window)
    alignment _ = alignment (undefined :: CChar)
    peek ptr = do
      uiElement' <- (#peek Window, uiElement) ptr
      name' <- (#peek Window, name) ptr
      pos' <- (#peek Window, pos) ptr
      size' <- (#peek Window, size) ptr
      return $ Window uiElement' name' pos' size'
    poke ptr (Window uiElement' name' pos' size') = do
        (#poke Window, uiElement) ptr uiElement'
        (#poke Window, name) ptr name'
        (#poke Window, pos) ptr pos'
        (#poke Window, size) ptr size'

instance Storable Windows where
    sizeOf _ = (#size Windows)
    alignment _ = alignment (undefined :: Window)
    peek ptr = do
      elements' <- (#peek Windows, elements) ptr
      return $ Windows elements'
    poke ptr (Windows elements') = do
        (#poke Windows, elements) ptr elements'
