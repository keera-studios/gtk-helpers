module Graphics.UI.Gtk.Display.ZoomView where

import Control.Exception as E
import Control.Monad
import Control.Monad.Trans(liftIO)
import Data.Maybe
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import Graphics.UI.Gtk.Gdk.Pixbuf
import System.Glib.Types

data ZoomView = ZoomView Image

instance WidgetClass ZoomView
instance ObjectClass ZoomView
instance GObjectClass ZoomView where
  toGObject (ZoomView da) = toGObject da
  unsafeCastGObject = ZoomView . unsafeCastGObject

zoomViewNewOf :: WidgetClass w => ScrolledWindow -> w -> (Int, Int) -> IO ZoomView
zoomViewNewOf scroll widget sz@(w, h) = do
  -- Create widget
  -- (w,h) <- widgetGetSize widget
  pb <- pixbufNew ColorspaceRgb True 8 1 1
  pixbufFill pb 255 255 255 0
  img <- imageNewFromPixbuf pb
  -- Install handlers
  widget `after` realize $ redisplayProp scroll widget img sz
  -- widget `after` noExposeEvent $ liftIO (redisplayProp widget img sz prop) >> return False
  img `on` exposeEvent $ liftIO (redisplayProp scroll widget img sz) >> return False
  return $ ZoomView img

-- Redisplay
redisplayProp :: WidgetClass w => ScrolledWindow -> w -> Image -> (Int, Int) -> IO ()
redisplayProp scroll widget img sz = do
  (w2,h2) <- widgetGetSize widget
  dw <- widgetGetDrawWindow widget
  drawWindowInvalidateRect dw (Rectangle 0 0 w2 h2) True
  drawWindowProcessUpdates dw True
  dw2 <- widgetGetDrawWindow widget
  print =<< drawableGetClipRegion dw2
  widgetQueueDraw widget
  postGUIAsync $ redisplayProp' scroll widget img sz

redisplayProp' :: WidgetClass w => ScrolledWindow -> w -> Image -> (Int, Int) -> IO ()
redisplayProp' scroll widget img sz@(w,h) = do
  -- threadsEnter
  sz2@(w2,h2) <- widgetGetSize widget
  realizd1 <- widgetGetRealized img
  realizd2 <- widgetGetRealized widget
  when (realizd1 && realizd2) $ do
    -- hadj <- scrolledWindowGetHAdjustment scroll
    -- vadj <- scrolledWindowGetVAdjustment scroll

    -- hLower <- adjustmentGetLower hadj
    -- hUpper <- adjustmentGetUpper hadj
    -- hPage  <- adjustmentGetPageSize hadj

    -- vLower <- adjustmentGetLower vadj
    -- vUpper <- adjustmentGetUpper vadj
    -- vPage  <- adjustmentGetPageSize vadj

    -- print (hLower, hUpper, hPage, vLower, vUpper, vPage)
    -- pb <- pixbufNew ColorspaceRgb True 8 w h
    -- pixbufFill pb 255 255 255 0
    -- imageSetFromPixbuf img pb

    wgdw <- widgetGetDrawWindow widget
    -- sz2@(w',h') <- drawableGetSize wgdw
    let r = Rectangle 0 0 w2 h2
    mPm <- widgetGetSnapshot widget r -- $ Rectangle (-100) (-100) 300 300
    -- pb <- pixbufGetFromDrawable wgdw r
    maybe (return ()) (\pm -> do
        print =<< drawableGetSize pm
        -- pm <- pixmapNew (Nothing :: Maybe DrawWindow) w2 h2 (Just 24)
        gc <- gcNew pm
        -- drawPixbuf pm gc pb' 0 0 0 0 (-1) (-1) RgbDitherNone (-1) (-1)
        -- gcSetValues gc $ newGCValues { foreground = Color 65535 0 0 }
        -- let ((rx0,ry0),(rW,rH)) = zoomBox (toInt hLower, toInt hUpper, toInt hPage) (toInt vLower, toInt vUpper, toInt vPage)
        -- coords <- getCoords scroll widget
        -- print r
        -- drawRectangle pm gc False rx0 ry0 rW rH
        pb'' <- pixbufGetFromDrawable pm r
        maybe (return ()) (\pb''' -> do
          print =<< pixbufGetWidth pb'''
          print =<< pixbufGetHeight pb'''
          print =<< widgetGetSize img
          -- let (dW,dH) = scalingSize sz2 sz
          -- pb2 <- pixbufScaleSimple pb''' dW dH InterpBilinear
          imageSetFromPixbuf img pb'''
         ) pb''
      ) mPm
    return ()
  -- threadsLeave
  where toInt = round

scalingSize :: (Int,Int) -> (Int,Int) -> (Int,Int)
scalingSize s       (0 ,_)  = s
scalingSize s       (_ ,0)  = s
scalingSize (oW,oH) (dW,dH) = (fW,fH)
 where fW = round (fromIntegral oW * scale)
       fH = round (fromIntegral oH * scale)
       scale = min scaleH scaleW
       scaleW = fromIntegral dW / fromIntegral oW
       scaleH = fromIntegral dH / fromIntegral oH

zoomBox :: (Int, Int, Int) -> (Int, Int, Int) -> ((Int, Int), (Int, Int))
zoomBox (hLower, hUpper, hPage) (vLower, vUpper, vPage) = ((hLower, vLower), (hPage, vPage))
