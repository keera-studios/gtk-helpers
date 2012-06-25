module Graphics.UI.Gtk.Board.TiledBoard where

import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import Data.Maybe (isJust)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import System.Glib.Types
import Data.IORef
import Data.Array.IO
import Data.Board

data Board index player tile piece = Board
  { boardDrawingArea :: DrawingArea
  , boardTiles       :: GameBoard index tile
  , boardPieces      :: GameBoard index (player, piece)
  , tilePixmaps      :: PixmapsFor tile
  , piecePixmaps     :: PixmapsFor (player, piece)
  , tileSize         :: (Int, Int)
  }

instance WidgetClass (Board index player tile piece)
instance ObjectClass (Board index player tile piece)
instance GObjectClass (Board index player tile piece) where
  toGObject = toGObject . boardDrawingArea
  -- unsafeCastGObject = undefined

type PixmapsFor a = a -> Pixbuf

boardNew :: Ix index
         => [(index,index,tile)] -> PixmapsFor tile -> PixmapsFor (player, piece) -> (Int, Int)
         -> IO (Board index player tile piece)
boardNew tileList tilePixs piecePixs tileSize = do
  da <- drawingAreaNew

  tb <- gameBoardNew tileList
  pb <- gameBoardNewEmpty (map (\(x,y,_) -> (x,y)) tileList)

  let board = Board da tb pb tilePixs piecePixs tileSize

  (pixW, pixH) <- boardGetPixelSize board

  da `on` realize $ widgetSetSizeRequest da pixW pixH
  da `on` exposeEvent $ liftIO (boardRefresh board) >> return False
  widgetAddEvents da [ButtonPressMask]

  return board

boardGetPiece :: Ix index => (index, index) -> Board index player tile piece -> IO (Maybe (player, piece))
boardGetPiece pos board = gameBoardGetPiece pos (boardPieces board)

boardSetPiece :: Ix index => (index, index) -> (player, piece) -> Board index player tile piece -> IO ()
boardSetPiece pos piece board = do
  -- check that there's a tile there
  posOk <- fmap isJust $ gameBoardGetPiece pos $ boardTiles board
  when posOk $ do
    -- if there is, place the piece on the pieces board
    gameBoardSetPiece pos piece (boardPieces board) 
    -- refresh the image
    boardRefresh board

boardRemovePiece :: Ix index => (index, index) -> Board index player tile piece -> IO ()
boardRemovePiece pos board = do
  -- check that there's a tile there
  posOk <- fmap isJust $ gameBoardGetPiece pos $ boardTiles board
  when posOk $ do
    -- if there is, remove the piece from the pieces board
    gameBoardRemovePiece pos (boardPieces board) 
    -- refresh the image
    boardRefresh board

boardMovePiece :: Ix index => (index, index) -> (index, index) -> Board index player tile piece -> IO ()
boardMovePiece posO posD board = do
  -- check that there's a tile there
  posOrigOk <- fmap isJust $ gameBoardGetPiece posO $ boardTiles board
  posDestOk <- fmap isJust $ gameBoardGetPiece posD $ boardTiles board
  when (posOrigOk && posDestOk) $ do
    -- Move the piece
    gameBoardMovePiece posO posD $ boardPieces board
    -- Refresh the UI
    boardRefresh board

boardFoldM :: (Ix index) => Board index player tile piece -> (b -> ((index,index), (player, piece)) -> IO b) -> b -> IO b
boardFoldM board f def = gameBoardFoldM (boardPieces board) f def

boardRefresh :: Ix index => Board index player tile piece -> IO ()
boardRefresh board = do 
   dw <- widgetGetDrawWindow (boardDrawingArea board)

   (w,h) <- widgetGetSize (boardDrawingArea board)

   drawWindowBeginPaintRect dw (Rectangle 0 0 w h)

   -- Clear Drawing area
   drawWindowClear dw
   
   -- Draw tiles
   drawPixmaps dw (tileSize board) (boardTiles board) (tilePixmaps board)

   -- Draw Pieces
   drawPixmaps dw (tileSize board) (boardPieces board) (piecePixmaps board)

   drawWindowEndPaint dw

boardOnClick :: Ix index => Board index player tile piece -> ((index, index) -> IO ()) -> IO()
boardOnClick board p = do
  (boardDrawingArea board) `on` buttonPressEvent $ clickHandler board p
  return ()

clickHandler :: Ix index => Board index player tile piece -> ((index, index) -> IO ()) -> EventM EButton Bool
clickHandler board p = do
  (x,y) <- eventCoordinates
  liftIO $ do
    let (tileW, tileH) = tileSize board
        tileCol = round x `div` tileW
        tileRow = round y `div` tileH
    let (GameBoard array) = boardTiles board
    ((xm, ym), (xM, yM)) <- getBounds $ array
    let xs = range (xm, xM)
        ys = range (ym, yM)
    when (inRange (0, length xs - 1) tileCol && inRange (0, length ys - 1) tileRow) $
      p ((head (drop tileCol xs)), (head (drop tileRow ys)))
    return False
    
boardGetPixelSize :: Ix index => Board index player tile piece -> IO (Int, Int)
boardGetPixelSize board = do
  let (GameBoard array) = boardTiles board

  ((xm, ym), (xM, yM)) <- getBounds $ array
  let htiles = rangeSize (xm, xM)
      vtiles = rangeSize (ym, yM)
      (tileW, tileH) = tileSize board

  return (htiles * tileW, vtiles * tileH)

drawPixmaps :: (Ix index, DrawableClass d) => d -> (Int, Int) -> GameBoard index e -> PixmapsFor e -> IO()
drawPixmaps d tileSize@(tw, th) gameBoard@(GameBoard array) pixmaps = do
  gc <- gcNew d
  ((xm, ym), (xM, yM)) <- getBounds array

  let -- paintPixmap :: (index,index) -> e -> IO ()
      paintPixmap (x,y) elem = do
          let pixmap = pixmaps elem
              ix     = index (xm, xM) x
              iy     = index (ym, yM) y
              posX   = ix * tw
              posY   = iy * th
          drawPixbuf d gc pixmap 0 0 posX posY (-1) (-1) RgbDitherNone (-1) (-1)
      
  gameBoardFoldM_ gameBoard paintPixmap
