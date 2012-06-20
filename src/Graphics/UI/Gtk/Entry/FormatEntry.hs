module Graphics.UI.Gtk.Entry.FormatEntry
   ( FormatEntry
   , formatEntryNew
   , formatEntrySetColor
   , formatEntryGetColor
   , formatEntrySetCheckFunction
   , formatEntryGetCheckFunction
   , formatEntryHasCorrectFormat
   , formatEntryColor
   , formatEntryCheckFunction
   )
  where

import Control.Monad.Trans (liftIO)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Entry.HighlightedEntry
import System.Glib.Types
import Data.IORef

-- Higlighted Entries
data FormatEntry = FormatEntry HighlightedEntry (IORef FormatEntryParams)
type FormatEntryParams = String -> Bool

formatEntryNew :: (String -> Bool) -> IO FormatEntry
formatEntryNew checkF = do
  entry <- highlightedEntryNew
  defaultParamsRef <- newIORef checkF
  let formatEntry = FormatEntry entry defaultParamsRef
  formatEntry `on` keyPressEvent $ liftIO (refreshEntry formatEntry) >> return False
  formatEntry `on` keyReleaseEvent $ liftIO (refreshEntry formatEntry) >> return False
  formatEntry `onCopyClipboard` refreshEntry formatEntry
  formatEntry `onCutClipboard` refreshEntry formatEntry
  formatEntry `onPasteClipboard` refreshEntry formatEntry
  return formatEntry

instance GObjectClass FormatEntry where
  toGObject (FormatEntry entry _) = toGObject entry
  unsafeCastGObject o = FormatEntry (unsafeCastGObject o) undefined
instance ObjectClass FormatEntry
instance WidgetClass FormatEntry
instance EntryClass FormatEntry

formatEntrySetColor :: FormatEntry -> Color -> IO ()
formatEntrySetColor (FormatEntry e _) color = highlightedEntrySetColor e color

formatEntryGetColor :: FormatEntry -> IO Color
formatEntryGetColor (FormatEntry e _) = highlightedEntryGetColor e

formatEntrySetCheckFunction :: FormatEntry -> (String -> Bool) -> IO ()
formatEntrySetCheckFunction fe@(FormatEntry _ params) checkF = do
  writeIORef params checkF
  refreshEntry fe

formatEntryGetCheckFunction :: FormatEntry -> IO (String -> Bool)
formatEntryGetCheckFunction (FormatEntry _ params) =
  readIORef params

-- Repaints the entry using the current color, or resets the
-- default style if no warning has to be given
refreshEntry :: FormatEntry -> IO()
refreshEntry f@(FormatEntry entry params) = do
  correct <- formatEntryHasCorrectFormat f
  highlightedEntrySetStatus entry (not correct)

formatEntryColor :: Attr FormatEntry Color
formatEntryColor = newAttr getter setter
  where getter (FormatEntry e _)   = highlightedEntryGetColor e
        setter (FormatEntry e _) v = highlightedEntrySetColor e v

formatEntryCheckFunction :: Attr FormatEntry (String -> Bool)
formatEntryCheckFunction = newAttr formatEntryGetCheckFunction formatEntrySetCheckFunction

formatEntryHasCorrectFormat :: FormatEntry -> IO Bool
formatEntryHasCorrectFormat f = do
  text <- entryGetText f
  func <- formatEntryGetCheckFunction f
  return $ func text
