module Graphics.UI.Gtk.Layout.Notebook.CloseableNotebook where

import Graphics.UI.Gtk

type CloseableNotebook = Notebook

closeableNotebookNew :: IO CloseableNotebook
closeableNotebookNew = notebookNew

closeableNotebookAppendPage :: (NotebookClass self, WidgetClass child) => self
 -> child -- ^ @child@ - the 'Widget' to use as the contents of the page.
 -> String -- ^ @tabLabel@ - the label for the page
 -> IO Int -- ^ returns the index (starting from 0) of the appended page in
             -- the notebook, or -1 if function fails
closeableNotebookAppendPage nb ch lbl = do
  -- Append page normally
  ix <- notebookAppendPage nb ch lbl

  -- Create new widget to go on the label's place
  hbox    <- hBoxNew False 5
  lblWdgt <- labelNew $ Just lbl
  imgWdgt <- imageNewFromStock stockClose IconSizeMenu
  boxPackStartDefaults hbox lblWdgt
  boxPackStartDefaults hbox imgWdgt

  -- widgetShowAll hbox

  -- Set new 'label'
  notebookSetTabLabel nb ch hbox

  -- Return the page index
  return ix
