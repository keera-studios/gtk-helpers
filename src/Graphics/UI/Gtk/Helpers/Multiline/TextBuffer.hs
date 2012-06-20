-- | Auxiliary functions userful to manipulate TextBuffers
module Graphics.UI.Gtk.Helpers.Multiline.TextBuffer where

-- External imports
import Control.Monad
import Graphics.UI.Gtk

-- | Updates a text buffer only if it's necessary (to avoid extra events)
textBufferUpdateText :: TextBuffer -> String -> IO ()
textBufferUpdateText bf s = do
 tx <- textBufferGetAllText bf True
 when (tx /= s) $ textBufferSetText bf s

-- | Gets all the text from a text buffer
textBufferGetAllText :: TextBuffer -> Bool -> IO String
textBufferGetAllText bf hidden = do
 si <- textBufferGetStartIter bf
 ei <- textBufferGetEndIter bf
 textBufferGetText bf si ei hidden
