-- | A collection of small functions to help retrieve
-- information from Glade files.

module Graphics.UI.Gtk.Extra.Builder
   ( onBuilder
   , loadInterface
   )
  where

import Graphics.UI.Gtk

-- | Graphics.UI.Gtk.builderGetObject with the arguments flipped
-- (Builder goes last).
onBuilder :: (GObjectClass cls) =>
               (GObject -> cls) -> String -> Builder -> IO cls
onBuilder f s b = builderGetObject b f s

-- | Returns a builder from which the objects in this part of the interface
-- can be accessed.
loadInterface :: String -> IO Builder
loadInterface fn = builderNew `incidentallyM` (`builderAddFromFile` fn)
-- It can be written in point-free style, but I'm not sure it's
-- clearer
-- loadInterface = incidentallyM builderNew . flip builderAddFromFile

-- | Sequences two monadic computations and returns the
-- result of the first.
incidentallyM :: Monad m => m a -> (a -> m b) -> m a 
incidentallyM op f = op >>= (\x -> (f x >> return x))
