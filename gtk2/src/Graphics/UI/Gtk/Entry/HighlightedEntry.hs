module Graphics.UI.Gtk.Entry.HighlightedEntry
   ( HighlightedEntry
   , highlightedEntryNew
   , highlightedEntrySetColor
   , highlightedEntryGetColor
   , highlightedEntrySetStatus
   , highlightedEntryGetStatus
   , highlightedEntryColor
   , highlightedEntryStatus
   )
  where

import Control.Monad.Trans (liftIO)
import Graphics.UI.Gtk
import System.Glib.Types
import Data.IORef

-- Higlighted Entries
data HighlightedEntry = HighlightedEntry Entry (IORef HighlightedEntryParams)
type HighlightedEntryParams = (Color, Bool)

highlightedEntryNew :: IO HighlightedEntry
highlightedEntryNew = do
  entry <- entryNew
  defaultParamsRef <- newIORef (Color 65000 32000 32000, False)
  return $ HighlightedEntry entry defaultParamsRef

instance GObjectClass HighlightedEntry where
  toGObject (HighlightedEntry entry _) = toGObject entry
  unsafeCastGObject o = HighlightedEntry (unsafeCastGObject o) undefined
instance ObjectClass HighlightedEntry
instance WidgetClass HighlightedEntry
instance EntryClass HighlightedEntry

highlightedEntrySetStatus :: HighlightedEntry -> Bool -> IO ()
highlightedEntrySetStatus he@(HighlightedEntry _ params) status = do
  modifyIORef params (\(c,_) -> (c,status))
  refreshBaseColor he

highlightedEntryGetStatus :: HighlightedEntry -> IO Bool
highlightedEntryGetStatus (HighlightedEntry _ params) = do
  (_,status) <- readIORef params
  return status

highlightedEntrySetColor :: HighlightedEntry -> Color -> IO ()
highlightedEntrySetColor he@(HighlightedEntry _ params) color = do
  modifyIORef params (\(_, s) -> (color, s))
  refreshBaseColor he

highlightedEntryGetColor :: HighlightedEntry -> IO Color
highlightedEntryGetColor (HighlightedEntry _ params) = do
  (color, _) <- readIORef params
  return color

-- Repaints the entry using the current color, or resets the
-- default style if no warning has to be given
refreshBaseColor :: HighlightedEntry -> IO()
refreshBaseColor (HighlightedEntry entry params) = do
  (color, status) <- readIORef params
  if status
   then mapM_ (\s -> widgetModifyBase entry s color) sensitiveStates
   else mapM_ (widgetRestoreBase entry) sensitiveStates
 where sensitiveStates = [ StateNormal, StateActive
                         , StateSelected, StatePrelight
                         ]

highlightedEntryStatus :: Attr HighlightedEntry Bool
highlightedEntryStatus = newAttr highlightedEntryGetStatus highlightedEntrySetStatus

highlightedEntryColor :: Attr HighlightedEntry Color
highlightedEntryColor = newAttr highlightedEntryGetColor highlightedEntrySetColor
