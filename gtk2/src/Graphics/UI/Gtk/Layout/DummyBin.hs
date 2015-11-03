module Graphics.UI.Gtk.Layout.DummyBin where

import Graphics.UI.Gtk
import System.Glib.Types

data DummyBin = DummyBin EventBox

instance WidgetClass DummyBin
instance ObjectClass DummyBin
instance GObjectClass DummyBin where
 toGObject (DummyBin x) = toGObject x
 unsafeCastGObject = DummyBin . unsafeCastGObject

dummyBinNew :: IO DummyBin
dummyBinNew = do
 evbox <- eventBoxNew
 return $ DummyBin evbox

instance ContainerClass DummyBin where
