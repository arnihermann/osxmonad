{-# LANGUAGE ForeignFunctionInterface #-}
module OSXMonad.Window where

import Foreign
import Foreign.C

#include "utils.h"

#if CGFLOAT_IS_DOUBLE
type ArchCFloat = CDouble
#else
type ArchCFloat = CFloat
#endif

foreign import ccall "ApplicationServices/ApplicationServices.h AXAPIEnabled"
  axAPIEnabled :: IO Bool

foreign import ccall "utils.h setWindowFocused"
  setWindowFocused :: Ptr Window -> IO ()

foreign import ccall "utils.h setWindow"
  setWindow :: Ptr Window -> IO ()

foreign import ccall "utils.h getWindows"
  getWindows :: Ptr Windows -> IO Int

foreign import ccall "utils.h freeWindows"
  freeWindows :: Ptr Windows -> IO ()

foreign import ccall "utils.h getFrame"
  getFrame :: Ptr CGPoint -> Ptr CGSize -> IO ()

foreign import ccall "utils.h setupEventCallback"
  setupEventCallback :: IO ()

foreign import ccall "utils.h &globalEvent"
  globalEvent :: Ptr Event

foreign import ccall "utils.h collectEvent"
  collectEvent :: IO ()

foreign import ccall "utils.h isSpaceTransitioning"
  isSpaceTransitioning :: IO Bool

data CGPoint = CGPoint { x :: ArchCFloat, y :: ArchCFloat } deriving Show
data CGSize = CGSize { width :: ArchCFloat, height :: ArchCFloat } deriving Show

data AXUIElement

data Window = Window {
      wid :: CInt,
      uiElement :: Ptr AXUIElement,
      name :: CString,
      pos :: CGPoint,
      size :: CGSize
    } deriving Show

data Event = Event {
      keyCode :: CInt,
      altKey :: CInt,
      commandKey :: CInt,
      controlKey :: CInt,
      shiftKey :: CInt
    } deriving Show

data Windows = Windows { elements :: Ptr (Ptr Window) } deriving Show

instance Storable CGPoint where
    sizeOf _ = (#size CGPoint)
    alignment _ = alignment (undefined :: ArchCFloat)
    peek ptr = do
      x' <- (#peek CGPoint, x) ptr
      y' <- (#peek CGPoint, y) ptr
      return $ CGPoint x' y'
    poke ptr (CGPoint x' y') = do
        (#poke CGPoint, x) ptr x'
        (#poke CGPoint, y) ptr y'

instance Storable CGSize where
    sizeOf _ = (#size CGSize)
    alignment _ = alignment (undefined :: ArchCFloat)
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
      wid' <- (#peek Window, wid) ptr
      uiElement' <- (#peek Window, uiElement) ptr
      name' <- (#peek Window, name) ptr
      pos' <- (#peek Window, pos) ptr
      size' <- (#peek Window, size) ptr
      return $ Window wid' uiElement' name' pos' size'
    poke ptr (Window wid' uiElement' name' pos' size') = do
        (#poke Window, wid) ptr wid'
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

instance Storable Event where
    sizeOf _ = (#size Event)
    alignment _ = alignment (undefined :: CInt)
    peek ptr = do
      keyCode' <- (#peek Event, keyCode) ptr
      altKey' <- (#peek Event, altKey) ptr
      commandKey' <- (#peek Event, commandKey) ptr
      controlKey' <- (#peek Event, controlKey) ptr
      shiftKey' <- (#peek Event, shiftKey) ptr
      return $ Event keyCode' altKey' commandKey' controlKey' shiftKey'
    poke ptr (Event keyCode' altKey' commandKey' controlKey' shiftKey') = do
        (#poke Event, keyCode) ptr keyCode'
        (#poke Event, altKey) ptr altKey'
        (#poke Event, commandKey) ptr commandKey'
        (#poke Event, controlKey) ptr controlKey'
        (#poke Event, shiftKey) ptr shiftKey'
