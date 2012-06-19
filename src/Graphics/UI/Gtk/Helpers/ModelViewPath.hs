{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}
module Graphics.UI.Gtk.Helpers.ModelViewPath where

import Graphics.UI.Gtk

class ViewWithPathSelection a b | a -> b where
  modelViewGetCursor :: a -> IO (TreePath, Maybe b)

instance ViewWithPathSelection IconView CellRenderer where
  modelViewGetCursor = iconViewGetCursor

instance ViewWithPathSelection TreeView TreeViewColumn where
  modelViewGetCursor = treeViewGetCursor

