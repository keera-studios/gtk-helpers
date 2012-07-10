module Graphics.UI.Gtk.Board.TiledBoard where

import Control.Monad (when, void)
import Control.Monad.Trans (liftIO)
import Data.Maybe (isJust)
import Data.Array.IO
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import System.Glib.Types

-- Local imports
import Data.Board.GameBoardIO

data Board index tile piece = Board
  { boardDrawingArea :: DrawingArea
  , boardTiles       :: GameBoard index tile
  , boardPieces      :: GameBoard index piece
  , tilePixmaps      :: PixmapsFor tile
  , piecePixmaps     :: PixmapsFor piece
  , tileSize         :: (Int, Int)
  }

instance WidgetClass (Board index tile piece)
instance ObjectClass (Board index tile piece)
instance GObjectClass (Board index tile piece) where
  toGObject           = toGObject . boardDrawingArea
  unsafeCastGObject x = Board (unsafeCastGObject x) undefined undefined undefined undefined undefined

type PixmapsFor a = a -> Pixbuf

boardNew :: Ix index
         => [(index,index,tile)] -> PixmapsFor tile -> PixmapsFor piece
         -> IO (Board index tile piece)
boardNew tileList tilePixs piecePixs = do
  da <- drawingAreaNew

  tb <- gameBoardNew tileList
  pb <- gameBoardNewEmpty (map (\(x,y,_) -> (x,y)) tileList)

  ts <- getTileSize tileList tilePixs

  let board = Board da tb pb tilePixs piecePixs ts

  (pixW, pixH) <- boardGetPixelSize board

  da `on` realize $ widgetSetSizeRequest da pixW pixH
  da `on` exposeEvent $ liftIO (boardRefresh board) >> return False
  widgetAddEvents da [ButtonPressMask]

  return board

getTileSize :: [(index, index, tile)] -> PixmapsFor tile -> IO (Int, Int)
getTileSize []          _    = return (0,0)
getTileSize ((_,_,t):_) pixs = do
  let pb = pixs t
  w  <- pixbufGetWidth pb
  h  <- pixbufGetHeight pb
  return (w,h)

boardGetPiece :: Ix index => (index, index) -> Board index tile piece -> IO (Maybe piece)
boardGetPiece pos board = gameBoardGetPiece pos (boardPieces board)

boardSetPiece :: Ix index => (index, index) -> piece -> Board index tile piece -> IO ()
boardSetPiece pos piece board = do
  boardSetPieceNoRefresh pos piece board
  boardInvalidate board
--   boardRefresh board

boardSetPieceNoRefresh :: Ix index => (index, index) -> piece -> Board index tile piece -> IO ()
boardSetPieceNoRefresh pos piece board = do
  -- check that there's a tile there
  posOk <- fmap isJust $ gameBoardGetPiece pos $ boardTiles board
  when posOk $ do
    -- if there is, place the piece on the pieces board
    gameBoardSetPiece pos piece (boardPieces board) 

boardRemovePiece :: Ix index => (index, index) -> Board index tile piece -> IO ()
boardRemovePiece pos board = do
  -- check that there's a tile there
  posOk <- fmap isJust $ gameBoardGetPiece pos $ boardTiles board
  when posOk $ do
    -- if there is, remove the piece from the pieces board
    gameBoardRemovePiece pos (boardPieces board) 
    -- refresh the image
    boardInvalidate board
    -- boardRefresh board

boardMovePiece :: Ix index => (index, index) -> (index, index) -> Board index tile piece -> IO ()
boardMovePiece posO posD board = do
  -- check that there's a tile there
  posOrigOk <- fmap isJust $ gameBoardGetPiece posO $ boardTiles board
  posDestOk <- fmap isJust $ gameBoardGetPiece posD $ boardTiles board
  when (posOrigOk && posDestOk) $ do
    -- Move the piece
    gameBoardMovePiece posO posD $ boardPieces board
    -- Refresh the UI
    boardInvalidate board
    -- boardRefresh board

boardInvalidate :: Ix index => Board index tile piece -> IO ()
boardInvalidate = widgetQueueDraw 

boardRefresh :: Ix index => Board index tile piece -> IO ()
boardRefresh board = do 
  realized <- widgetGetRealized board
  when realized $ do
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

boardOnClick :: Ix index => Board index tile piece -> ((index, index) -> IO ()) -> IO()
boardOnClick board p = void $
  (boardDrawingArea board) `on` buttonPressEvent $ clickHandler board p

clickHandler :: Ix index => Board index tile piece -> ((index, index) -> IO ()) -> EventM EButton Bool
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
    
boardGetPixelSize :: Ix index => Board index tile piece -> IO (Int, Int)
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
  ((xm, ym), (xM, yM)) <- gameBoardGetBoundaries gameBoard -- getBounds array

  let paintPixmap (x,y) elem = do
          let pixmap = pixmaps elem
              ix     = index (xm, xM) x
              iy     = index (ym, yM) y
              posX   = ix * tw
              posY   = iy * th
          drawPixbuf d gc pixmap 0 0 posX posY (-1) (-1) RgbDitherNone (-1) (-1)
      
  gameBoardMapM_ gameBoard paintPixmap

boardFoldM :: (Ix index) => Board index tile piece -> (b -> ((index,index), piece) -> IO b) -> b -> IO b
boardFoldM board f def = gameBoardFoldM (boardPieces board) f def

boardClear :: Ix index => Board index tile piece -> IO ()
boardClear board = do
  gameBoardClear (boardPieces board)
  boardInvalidate board

--   boardRefresh board

boardLoad :: Ix index => Board index tile piece -> [((index, index), piece)] -> IO()
boardLoad board pieces = do
  gameBoardClear (boardPieces board)
  mapM_ (\(pos, piece) -> boardSetPieceNoRefresh pos piece board) pieces
  boardRefresh board
