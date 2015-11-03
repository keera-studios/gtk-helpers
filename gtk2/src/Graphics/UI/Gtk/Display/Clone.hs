module Graphics.UI.Gtk.Display.Clone where

import Control.Exception as E
import Control.Monad
import Control.Monad.Trans(liftIO)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import Graphics.UI.Gtk.Gdk.Pixbuf
import System.Glib.Types

data Clone = Clone Image

instance WidgetClass Clone
instance ObjectClass Clone
instance GObjectClass Clone where
  toGObject (Clone da) = toGObject da
  unsafeCastGObject = Clone . unsafeCastGObject

cloneNewOf :: WidgetClass w => w -> IO Clone
cloneNewOf widget = do
  (w,h) <- widgetGetSize widget
  pb <- pixbufNew ColorspaceRgb False 8 w h
  img <- imageNewFromPixbuf pb
  widget `after` realize $ redisplay widget img
  img `on` exposeEvent $ liftIO (redisplay widget img) >> return False
  return $ Clone img

redisplay :: WidgetClass w => w -> Image -> IO ()
redisplay widget img = postGUIAsync $ redisplay' widget img

redisplay' :: WidgetClass w => w -> Image -> IO ()
redisplay' widget img = do
  threadsEnter
  realizd1 <- widgetGetRealized img
  realizd2 <- widgetGetRealized widget
  when (realizd1 && realizd2) $ do
    displayGetDefault >>= \x -> maybe (return ()) displayFlush x
    wgdw <- widgetGetDrawWindow widget
    (w',h') <- drawableGetSize wgdw
    let r = Rectangle 0 0 w' h'
    pb <- pixbufGetFromDrawable wgdw r
    maybe (return ()) (imageSetFromPixbuf img) pb
  threadsLeave

cloneNewScaledOf :: WidgetClass w => w -> (Int, Int) -> Bool -> IO Clone
cloneNewScaledOf widget sz prop = do
  (w,h) <- widgetGetSize widget
  pb <- pixbufNew ColorspaceRgb True 8 w h
  img <- imageNewFromPixbuf pb
  widget `after` realize $ redisplayProp widget img sz prop
  -- widget `after` noExposeEvent $ liftIO (redisplayProp widget img sz prop) >> return False
  img `on` exposeEvent $ liftIO (redisplayProp widget img sz prop) >> return False
  return $ Clone img
 
redisplayProp :: WidgetClass w => w -> Image -> (Int, Int) -> Bool -> IO ()
redisplayProp widget img sz prop = postGUIAsync $ redisplayProp' widget img sz prop

redisplayProp' :: WidgetClass w => w -> Image -> (Int, Int) -> Bool -> IO ()
redisplayProp' widget img sz@(w,h) prop = do
  threadsEnter
  sz2@(w2,h2) <- widgetGetSize widget
  realizd1 <- widgetGetRealized img
  realizd2 <- widgetGetRealized widget
  when (realizd1 && realizd2) $ do
    wgdw <- widgetGetDrawWindow widget
    sz2@(w',h') <- drawableGetSize wgdw
    let r = Rectangle 0 0 w' h'
    pb <- pixbufGetFromDrawable wgdw r
    maybe (return ()) (\pb' -> do
        let (dW,dH) = scalingSize sz2 sz prop
        pb2 <- pixbufScaleSimple pb' dW dH InterpBilinear
        imageSetFromPixbuf img pb2
      ) pb
    return ()
  threadsLeave

scalingSize :: (Int,Int) -> (Int,Int) -> Bool -> (Int,Int)
scalingSize s       (0 ,_)  _     = s
scalingSize s       (_ ,0)  _     = s
scalingSize (oW,oH) (dW,dH) False = (dW,dH)
scalingSize (oW,oH) (dW,dH) True  = (fW,fH)
 where fW = round (fromIntegral oW * scale)
       fH = round (fromIntegral oH * scale)
       scale = min scaleH scaleW
       scaleW = fromIntegral dW / fromIntegral oW
       scaleH = fromIntegral dH / fromIntegral oH
