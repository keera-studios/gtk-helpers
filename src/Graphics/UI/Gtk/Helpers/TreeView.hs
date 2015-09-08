module Graphics.UI.Gtk.Helpers.TreeView where

-- External imports
import Control.Monad
import Data.Maybe
import Graphics.UI.Gtk

type TypedTreeView a = (TreeView, ListStore a)

addTextColumn :: (TreeModelClass (model row), TypedTreeModelClass model)
                => TreeView -> model row -> (row -> Maybe String) -> IO()
addTextColumn tv st f = void $ do
  col <- treeViewColumnNew
  renderer <- cellRendererTextNew
  cellLayoutPackStart col renderer True
  cellLayoutSetAttributes col renderer st $ map (cellText :=).maybeToList.f
  treeViewAppendColumn tv col

addPixbufColumn :: (TreeModelClass (model row), TypedTreeModelClass model)
                => TreeView -> model row -> (row -> Maybe Pixbuf) -> IO()
addPixbufColumn tv st f = void $ do
  icon <- treeViewColumnNew
  renderIcon <- cellRendererPixbufNew
  cellLayoutPackStart icon renderIcon True
  cellLayoutSetAttributes icon renderIcon st $ map (cellPixbuf :=).maybeToList.f
  treeViewAppendColumn tv icon

addPixbufTextDoubleColumn :: (TreeModelClass (model row), TypedTreeModelClass model)
                          => TreeView -> model row -> (row -> (Maybe Pixbuf, Maybe String)) -> IO()
addPixbufTextDoubleColumn tv st f = void $ do
  col <- treeViewColumnNew

  renderIcon <- cellRendererPixbufNew
  cellLayoutPackStart col renderIcon False
  cellLayoutSetAttributes col renderIcon st $ map (cellPixbuf :=).maybeToList.fst.f

  renderer <- cellRendererTextNew
  cellLayoutPackEnd col renderer True
  cellLayoutSetAttributes col renderer st $ map (cellText :=).maybeToList.snd.f

  treeViewAppendColumn tv col

synchroniseTreeViewAndNotebook :: TreeView -> Notebook -> ([TreePath] -> Maybe Int) -> IO()
synchroniseTreeViewAndNotebook tv nb f = do
  tr <- treeViewGetSelectedPath tv
  maybe (return ()) (\v -> set nb [ notebookPage := v ]) (f tr)

treeViewGetSelectedPath :: TreeViewClass tv => tv -> IO [[Int]]
treeViewGetSelectedPath =
  treeSelectionGetSelectedRows <=< treeViewGetSelection

treeViewGetSelected :: TreeViewClass tv => tv -> ListStore a -> IO (Maybe a)
treeViewGetSelected tv ls = do
  tr <- treeViewGetSelectedPath tv
  case tr of
   [[x]] -> fmap Just $ listStoreGetValue ls x
   _     -> return Nothing

