module Graphics.UI.Gtk.Layout.Notebook.ComponentInfoNoteBook where

import Data.IORef
import Graphics.UI.Gtk

type ComponentInfoNoteBook = (NoteBook, IORef [(String,ComponentInfo,ComponentViewInfo)])

type ComponentInfo = (String, [String])

type ComponentViewInfo = (HBox,IconView, TextView)

componentInfoWidget :: IO NoteBook
componentInfoWidget = fst

componentInfoNew :: IO ComponentInfoNoteBook
componentInfoNew = do
  nb <- notebookNew 
  cis <- newIORef []
  return (nb,cis)
  
componentInfoAddPage :: ComponentInfoNoteBook -> String -> ComponentInfo -> IO ()
componentInfoAddPage (nb, info) = undefined

componentInfoRemovePage :: ComponentInfoNoteBook -> String -> IO ()
componentInfoRemovePage (nb, infoR) = do
  info <- readIORef infoR
  let viewInfo = undefined
  return ()

componentInfoUpdatePage :: ComponentInfoNoteBook -> String -> ComponentInfo -> IO ()
componentInfoUpdatePage = undefined

componentInfoOnPageClose :: ComponentInfoNoteBook -> (String -> IO ()) -> IO()
componentInfoOnPageClose = undefined
