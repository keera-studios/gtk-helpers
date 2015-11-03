module Graphics.UI.Gtk.Helpers.Pixbuf where

import Graphics.UI.Gtk
import Data.Word

safePixbufComposite :: Pixbuf -> Pixbuf
                    -> Int -> Int -- position
                    -> Int -> Int -- something else?
                    -> InterpType
                    -> Word8
                    -> IO ()
safePixbufComposite pbO pbD x y x' y' interp alpha = do
  w <- pixbufGetWidth pbO
  h <- pixbufGetHeight pbO
  w' <- pixbufGetWidth pbD
  h' <- pixbufGetHeight pbD
  safePixbufComposite' pbO pbD x y x' y' w h w' h' interp alpha

safePixbufComposite' :: Pixbuf -> Pixbuf
                    -> Int -> Int -- position
                    -> Int -> Int -- something else?
                    -> Int -> Int -- original image size
                    -> Int -> Int -- destination image size
                    -> InterpType
                    -> Word8
                    -> IO ()
safePixbufComposite' pbO pbD x y x' y' w h w' h' interp alpha
  | x + w < 0 || y + h < 0 || w < 0 || h < 0
  = return ()
  | x < 0 || y < 0
  = do let diffX = if x > 0 then 0 else abs x
           diffY = if y > 0 then 0 else abs y
           newX  = if x < 0 then 0 else x
           newY  = if y < 0 then 0 else y
           newX' = if x < 0 then 0 else x'
           newY' = if y < 0 then 0 else y'
       pbO' <- pixbufNew ColorspaceRgb True 8 (w - diffX) (h - diffY)
       pixbufCopyArea pbO diffX diffY (w - diffX) (h - diffY) pbO' 0 0
       safePixbufComposite' pbO' pbD
                            newX newY
                            newX' newY'
                            (w - diffX) (h - diffY)
                            w' h'
                            interp alpha
  | otherwise
  = do let realw = if (x + w) > w' then w' - x else w
           realh = if (y + h) > h' then h' - y else h
       pixbufComposite pbO pbD x y realw realh (fI x') (fI y') 1 1 interp alpha
 where fI = fromIntegral

pixbufNewEmpty :: Int -> Int -> IO Pixbuf
pixbufNewEmpty w h = do
  pb <- pixbufNew ColorspaceRgb True 8 w h
  pixbufFill pb 0 0 0 1
  return pb

pixbufNewWithBGColor :: Bool -> Int -> Int -> (Word8, Word8, Word8) -> IO Pixbuf
pixbufNewWithBGColor alpha w h (r,g,b) = do
  pb <- pixbufNew ColorspaceRgb alpha 8 w h
  pixbufFill pb r g b 255
  return pb
