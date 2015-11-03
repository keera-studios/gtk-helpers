module Graphics.UI.Gtk.Layout.EitherWidget where

import Control.Monad
import Data.IORef
import Graphics.UI.Gtk
import System.Glib.Types

data EitherWidget a b   = EitherWidget Notebook (IORef EitherWidgetParams)
type EitherWidgetParams = Bool

instance WidgetClass (EitherWidget a b)
instance ObjectClass (EitherWidget a b)
instance GObjectClass (EitherWidget a b) where
  toGObject (EitherWidget nb _) = toGObject nb
  unsafeCastGObject o = EitherWidget (unsafeCastGObject o) undefined
 
eitherWidgetNew :: (WidgetClass a, WidgetClass b) => a -> b -> IO (EitherWidget a b)
eitherWidgetNew wL wR = do
  nb <- notebookNew
  _  <- notebookAppendPage nb wL ""
  _  <- notebookAppendPage nb wR ""
  notebookSetShowTabs nb False
  params <- newIORef True
  return $ EitherWidget nb params

eitherWidgetLeftActivated :: Attr (EitherWidget a b) Bool
eitherWidgetLeftActivated = newAttr getter setter
  where getter (EitherWidget _ paramsR)    = readIORef paramsR
        setter (EitherWidget nb paramsR) v = do
                  params <- readIORef paramsR
                  when (v /= params) $ do let upd = if v then 0 else 1
                                          notebookSetCurrentPage nb upd
                                          writeIORef paramsR v

eitherWidgetRightActivated :: Attr (EitherWidget a b) Bool
eitherWidgetRightActivated = newAttr getter setter
  where getter w   = fmap not $ get w eitherWidgetLeftActivated
        setter w v = set w [ eitherWidgetLeftActivated := not v ]

eitherWidgetToggle :: EitherWidget a b -> IO()
eitherWidgetToggle w = set w [ eitherWidgetLeftActivated :~ not ]
