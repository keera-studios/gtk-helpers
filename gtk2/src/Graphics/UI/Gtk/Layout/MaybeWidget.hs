module Graphics.UI.Gtk.Layout.MaybeWidget where

import Control.Monad
import Data.IORef
import Graphics.UI.Gtk
import System.Glib.Types

data MaybeWidget a     = MaybeWidget Notebook a Label (IORef MaybeWidgetParams)
type MaybeWidgetParams = Bool

instance WidgetClass (MaybeWidget a)
instance ObjectClass (MaybeWidget a)
instance GObjectClass (MaybeWidget a) where
  toGObject (MaybeWidget nb _ _ _) = toGObject nb
  unsafeCastGObject o = MaybeWidget (unsafeCastGObject o) undefined undefined undefined
 
maybeWidgetNewWithLabel :: (WidgetClass a) => a -> Maybe String -> IO (MaybeWidget a)
maybeWidgetNewWithLabel w label = do
  lblW <- labelNew label
  nb <- notebookNew
  _  <- notebookAppendPage nb lblW ""
  _  <- notebookAppendPage nb w ""
  notebookSetShowTabs nb False
  params <- newIORef False
  return $ MaybeWidget nb w lblW params

maybeWidgetGetWidget :: MaybeWidget a -> a
maybeWidgetGetWidget (MaybeWidget _ a _ _) = a
 
maybeWidgetLabelText :: Attr (MaybeWidget a) String
maybeWidgetLabelText = newAttr getter setter
  where getter (MaybeWidget _ _ lblW _)   = get lblW labelLabel
        setter (MaybeWidget _ _ lblW _) s = set lblW [ labelLabel := s ]

maybeWidgetActivated :: Attr (MaybeWidget a) Bool
maybeWidgetActivated = newAttr getter setter
  where getter (MaybeWidget _ _ _ paramsR)    = readIORef paramsR
        setter (MaybeWidget nb _ _ paramsR) v = do
                  params <- readIORef paramsR
                  when (v /= params) $ do let upd = if v then 1 else 0
                                          notebookSetCurrentPage nb upd
                                          writeIORef paramsR v

maybeWidgetToggle :: MaybeWidget a -> IO()
maybeWidgetToggle w = set w [ maybeWidgetActivated :~ not ]
