module Graphics.UI.Gtk.Layout.BackgroundContainer where

import Control.Monad.Trans (liftIO)
import Data.IORef
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import System.Glib.Types

data BackgroundContainer = BackgroundContainer EventBox (IORef (Maybe Pixbuf))

instance WidgetClass BackgroundContainer
instance ObjectClass BackgroundContainer
instance GObjectClass BackgroundContainer where
  toGObject (BackgroundContainer ev _) = toGObject ev
  unsafeCastGObject ev = (BackgroundContainer (unsafeCastGObject ev) undefined)

instance EventBoxClass BackgroundContainer
instance ContainerClass BackgroundContainer
instance BinClass BackgroundContainer

backgroundContainerNew :: IO BackgroundContainer
backgroundContainerNew = do
  ev  <- eventBoxNew
  ref <- newIORef Nothing
  return $ BackgroundContainer ev ref

backgroundContainerNewWithPicture :: FilePath -> IO BackgroundContainer
backgroundContainerNewWithPicture fp = do
  ev  <- eventBoxNew
  pb  <- pixbufNewFromFile fp
  ref <- newIORef (Just pb)
  let wdgt = BackgroundContainer ev ref
  wdgt `on` exposeEvent $ liftIO (backgroundExpose wdgt) >> return False
  return wdgt

backgroundContainerNewWithPixbuf :: Pixbuf -> IO BackgroundContainer
backgroundContainerNewWithPixbuf pb = do
  ev  <- eventBoxNew
  ref <- newIORef (Just pb)
  let wdgt = BackgroundContainer ev ref
  wdgt `on` exposeEvent $ liftIO (backgroundExpose wdgt) >> return False
  return wdgt

backgroundExpose :: BackgroundContainer -> IO ()
backgroundExpose (BackgroundContainer ev ref) = do
  dw <- widgetGetDrawWindow ev
  drawWindowClear dw
  pixbufM <- readIORef ref
  case pixbufM of
   Nothing -> return ()
   Just pb -> do sz@(w,h) <- widgetGetSize ev
                 pb' <- pixbufScaleSimple pb w h InterpBilinear
                 drawWindowBeginPaintRect dw (Rectangle 0 0 w h)
                 gc <- gcNew dw
                 drawPixbuf dw gc pb' 0 0 0 0 (-1) (-1) RgbDitherNone (-1) (-1)
                 drawWindowEndPaint dw

backgroundSetPicture :: BackgroundContainer -> Maybe FilePath -> IO()
backgroundSetPicture (BackgroundContainer ev ref) fpM = do
  pbM <- maybe (return Nothing) (fmap Just . pixbufNewFromFile) fpM
  writeIORef ref pbM

backgroundSetPixbuf :: BackgroundContainer -> Maybe Pixbuf -> IO()
backgroundSetPixbuf (BackgroundContainer ev ref) pbM =
  writeIORef ref pbM
