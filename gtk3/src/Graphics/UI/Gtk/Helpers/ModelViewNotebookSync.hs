-- | This module provides functions to alter the view in a notebook and a
-- maybewidget with a notebook inside based on what is selected in a model view
-- (icon view, list view, tree view)
--
-- This pattern appears very often in my programs. I like to use model views to
-- let the user choose what to see (instead of using the tab pages).
module Graphics.UI.Gtk.Helpers.ModelViewNotebookSync where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Helpers.ModelViewPath
import Graphics.UI.Gtk.Layout.MaybeWidget

-- | Keeps the page of a notebook in sync with the selection in a model view.
-- It uses an auxiliary function to determine which page to select.
modelViewNotebookSync :: (ViewWithPathSelection a c, NotebookClass b)
                            => a -> b -> (TreePath -> Int) -> IO()
modelViewNotebookSync modelView notebook syncF = do
  (path, _) <- modelViewGetCursor modelView
  let page = syncF path
  notebookSetCurrentPage notebook page

-- | Keeps the page of a notebook in sync with the selection in a model view.
-- It selects the page with the same number as the element selected in the
-- model view. If there's more than one selection, or if the model view is not
-- flat and the selected element is not at the first level, then it selects nothing.
modelViewNotebookSyncId :: (ViewWithPathSelection a c, NotebookClass b) => a -> b -> IO()
modelViewNotebookSyncId modelView notebook = do
  (path, _) <- modelViewGetCursor modelView
  case path of
   [page] -> notebookSetCurrentPage notebook page
   _      -> return ()

-- | Keeps the page of a notebook inside a maybe widget in sync with the
-- selection in a model view.
--
-- It uses an auxiliary function to determine which page to select. If the function
-- returns Nothing, then the maybe widget is deactivated.
modelViewMaybeNotebookSync :: (ViewWithPathSelection a c, NotebookClass b)
                            => a -> MaybeWidget b -> (TreePath -> Maybe Int) -> IO()
modelViewMaybeNotebookSync modelView maybeNotebook syncF = do
  (path, _) <- modelViewGetCursor modelView
  case syncF path of
    Nothing -> set maybeNotebook [ maybeWidgetActivated := False]
    Just p  -> do set maybeNotebook [ maybeWidgetActivated := True ]
                  notebookSetCurrentPage (maybeWidgetGetWidget maybeNotebook) p

-- | Keeps the page of a notebook inside a maybe widget in sync with the
-- selection in a model view.
--
-- It selects the page with the same number as the element selected in the
-- model view. If there's more than one selection, or if the model view is not
-- flat and the selected element is not at the first level, then it selects nothing.
modelViewMaybeNotebookSyncId :: (ViewWithPathSelection a c, NotebookClass b)
                             => a -> MaybeWidget b -> IO()
modelViewMaybeNotebookSyncId modelView maybeNotebook = do
  (path, _) <- modelViewGetCursor modelView
  case path of
   [page] -> do set maybeNotebook [ maybeWidgetActivated := True ]
                notebookSetCurrentPage (maybeWidgetGetWidget maybeNotebook) page
   _      -> set maybeNotebook [ maybeWidgetActivated := False ]
