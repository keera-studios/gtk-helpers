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
import System.Glib.Types
import Data.IORef

-- Higlighted Entries
data FormatEntry = FormatEntry Entry (IORef FormatEntryParams)
type FormatEntryParams = (Color, String -> Bool)

formatEntryNew :: (String -> Bool) -> IO FormatEntry
formatEntryNew checkF = do
  entry <- entryNew
  defaultParamsRef <- newIORef (Color 65000 32000 32000, checkF)
  let formatEntry = FormatEntry entry defaultParamsRef
  formatEntry `on` keyPressEvent $ liftIO (refreshBaseColor formatEntry) >> return False
  formatEntry `on` keyReleaseEvent $ liftIO (refreshBaseColor formatEntry) >> return False
  formatEntry `onCopyClipboard` refreshBaseColor formatEntry
  formatEntry `onCutClipboard` refreshBaseColor formatEntry
  formatEntry `onPasteClipboard` refreshBaseColor formatEntry
  return formatEntry

instance GObjectClass FormatEntry where
  toGObject (FormatEntry entry _) = toGObject entry
  unsafeCastGObject o = FormatEntry (unsafeCastGObject o) undefined
instance ObjectClass FormatEntry
instance WidgetClass FormatEntry
instance EntryClass FormatEntry

formatEntrySetColor :: FormatEntry -> Color -> IO ()
formatEntrySetColor he@(FormatEntry _ params) color = do
  modifyIORef params (\(_, f) -> (color, f))
  refreshBaseColor he

formatEntryGetColor :: FormatEntry -> IO Color
formatEntryGetColor (FormatEntry _ params) = do
  (color, _) <- readIORef params
  return color

formatEntrySetCheckFunction :: FormatEntry -> (String -> Bool) -> IO ()
formatEntrySetCheckFunction he@(FormatEntry _ params) checkF = do
  modifyIORef params (\(c, _) -> (c, checkF))

formatEntryGetCheckFunction :: FormatEntry -> IO (String -> Bool)
formatEntryGetCheckFunction (FormatEntry _ params) = do
  (_, checkF) <- readIORef params
  return checkF

-- Repaints the entry using the current color, or resets the
-- default style if no warning has to be given
refreshBaseColor :: FormatEntry -> IO()
refreshBaseColor f@(FormatEntry entry params) = do
  (color, _) <- readIORef params
  status <- formatEntryHasCorrectFormat f
  if status
   then mapM_ (widgetRestoreBase entry) sensitiveStates
   else mapM_ (\s -> widgetModifyBase entry s color) sensitiveStates
 where sensitiveStates = [ StateNormal, StateActive
                         , StateSelected, StatePrelight
                         ]

formatEntryColor :: Attr FormatEntry Color
formatEntryColor = newAttr formatEntryGetColor formatEntrySetColor

formatEntryCheckFunction :: Attr FormatEntry (String -> Bool)
formatEntryCheckFunction = newAttr formatEntryGetCheckFunction formatEntrySetCheckFunction

formatEntryHasCorrectFormat :: FormatEntry -> IO Bool
formatEntryHasCorrectFormat f = do
  text <- entryGetText f
  func <- formatEntryGetCheckFunction f
  return $ func text
