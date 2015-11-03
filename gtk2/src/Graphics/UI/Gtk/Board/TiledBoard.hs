module Graphics.UI.Gtk.Board.TiledBoard where

import Control.Monad (when, void)
import Control.Monad.Trans (liftIO)
import Data.Array.IO
import Data.IORef
import Data.Maybe (isJust, fromJust)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC (gcNew)
import System.Glib.Types

-- Local imports
import Data.Board.GameBoardIO

data Board index tile piece = Board
  { boardDrawingArea  :: DrawingArea
  , boardTiles        :: GameBoard index tile
  , boardPieces       :: GameBoard index piece
  , tilePixmaps       :: PixmapsFor tile
  , piecePixmaps      :: PixmapsFor piece
  , tileSize          :: (Int, Int)
  , background        :: IORef (Maybe (Pixbuf, SizeAdjustment))
  , overlay           :: IORef (Maybe (Pixbuf, SizeAdjustment))
  , dragEnabled       :: IORef Bool
  , draggingFrom      :: IORef (Maybe (index, index))
  , draggingTo        :: IORef (Maybe (index, index))
  , draggingMouseOrig :: IORef (Maybe (Int, Int))
  , draggingMousePos  :: IORef (Maybe (Int, Int))
  , movingStatus      :: IORef (Maybe (MovingStatus index))
  }

data MovingStatus index = MovingStatus
  { movingFrom   :: (index, index)
  , movingTo     :: (index, index)
  , stepsPerUnit :: Double
  , timePerUnit  :: Double
  , movingStep   :: Double
  }

instance WidgetClass (Board index tile piece)
instance ObjectClass (Board index tile piece)
instance GObjectClass (Board index tile piece) where
  toGObject           = toGObject . boardDrawingArea
  unsafeCastGObject x = Board (unsafeCastGObject x) undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined

type PixmapsFor a = a -> Pixbuf

data SizeAdjustment = SizeAdjustment

boardNew :: Ix index
         => [(index,index,tile)] -> PixmapsFor tile -> PixmapsFor piece
         -> IO (Board index tile piece)
boardNew tileList tilePixs piecePixs = do
  da <- drawingAreaNew

  tb <- gameBoardNew tileList
  pb <- gameBoardNewEmpty (map (\(x,y,_) -> (x,y)) tileList)

  ts <- getTileSize tileList tilePixs

  bg <- newIORef Nothing
  ov <- newIORef Nothing

  dragging  <- newIORef False
  draggingF <- newIORef Nothing
  draggingT <- newIORef Nothing
  draggingO <- newIORef Nothing
  draggingP <- newIORef Nothing

  movingSt <- newIORef Nothing

  let board = Board da tb pb tilePixs piecePixs ts bg ov
                    dragging draggingF draggingT draggingO draggingP
                    movingSt

  (pixW, pixH) <- boardGetPixelSize board

  da `on` realize $ widgetSetSizeRequest da pixW pixH
  da `on` exposeEvent $ liftIO (boardRefresh board) >> return False

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

    bg <- readIORef (background board)
    ov <- readIORef (overlay board)
  
    drawWindowBeginPaintRect dw (Rectangle 0 0 w h)
  
    -- Clear Drawing area
    drawWindowClear dw
    gc <- gcNew dw
    when (isJust bg) $ do
      ((posBgX, posBgY), bg') <- uncurry (adjustPixbuf (w,h)) (fromJust bg)
      drawPixbuf dw gc bg' 0 0 posBgX posBgY (-1) (-1) RgbDitherNone (-1) (-1)

    -- Dragging status (used to determine what to draw)
    posM   <- readIORef (draggingFrom board)
    mpOrig <- readIORef (draggingMouseOrig board)
    mpPos  <- readIORef (draggingMousePos board)

    -- Draw tiles
    drawPixmaps dw (tileSize board) (boardTiles board) (tilePixmaps board)
  
    -- Draw Pieces
    piecesBoard <- if isJust posM && isJust mpOrig && isJust mpPos
                    then do pieces' <- gameBoardClone $ boardPieces board
                            gameBoardRemovePiece (fromJust posM) pieces'
                            return pieces'
                    else return $ boardPieces board
             
    -- drawPixmaps dw (tileSize board) (boardPieces board) (piecePixmaps board)
    drawPixmaps dw (tileSize board) piecesBoard (piecePixmaps board)

    -- Draw moving piece
    when (isJust posM && isJust mpOrig && isJust mpPos) $ do
      pieceM <- boardGetPiece (fromJust posM) board
      when (isJust pieceM) $ do
        let pb = piecePixmaps board (fromJust pieceM)
        let (mpPosX, mpPosY)   = fromJust mpPos
            (mpOrigX, mpOrigY) = fromJust mpOrig
            (x,y) = (mpPosX - mpOrigX, mpPosY - mpOrigY)
        drawPixbuf dw gc pb 0 0 x y (-1) (-1) RgbDitherNone (-1) (-1)

    when (isJust ov) $ do
      ((posOvX, posOvY), ov') <- uncurry (adjustPixbuf (w,h)) (fromJust ov)
      drawPixbuf dw gc ov' 0 0 posOvX posOvY (-1) (-1) RgbDitherNone (-1) (-1)
  
    drawWindowEndPaint dw

-- FIXME: To be completed
adjustPixbuf :: (Int, Int) -> Pixbuf -> SizeAdjustment -> IO ((Int, Int), Pixbuf)
adjustPixbuf _ pb _ = return ((0,0), pb)

mouseMotionHandler :: Ix index => Board index tile piece -> ((index, index) -> EventM EMotion Bool) -> EventM EMotion Bool
mouseMotionHandler board p = do
  coords <- eventCoordinates
  pos <- liftIO $ getMouseCoordinates board coords
  maybe (return False) p pos

mouseButtonHandler :: Ix index => Board index tile piece -> ((index, index) -> EventM EButton Bool) -> EventM EButton Bool
mouseButtonHandler board p = do
  coords <- eventCoordinates
  pos <- liftIO $ getMouseCoordinates board coords
  maybe (return False) p pos

getMouseCoordinates :: Ix index => Board index tile piece -> (Double, Double) -> IO (Maybe (index, index))
getMouseCoordinates board (x,y) = do
  let (tileW, tileH) = tileSize board
      tileCol = round x `div` tileW
      tileRow = round y `div` tileH
  let (GameBoard array) = boardTiles board
  ((xm, ym), (xM, yM)) <- getBounds $ array
  let xs = range (xm, xM)
      ys = range (ym, yM)
  if (inRange (0, length xs - 1) tileCol && inRange (0, length ys - 1) tileRow)
      then return $ Just ((head (drop tileCol xs)), (head (drop tileRow ys)))
      else return Nothing

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
  ((xm, ym), (xM, yM)) <- gameBoardGetBoundaries gameBoard

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

boardLoad :: Ix index => Board index tile piece -> [((index, index), piece)] -> IO()
boardLoad board pieces = do
  gameBoardClear (boardPieces board)
  mapM_ (\(pos, piece) -> boardSetPieceNoRefresh pos piece board) pieces
  boardRefresh board

boardOnClick :: Ix index => Board index tile piece -> ((index, index) -> IO ()) -> IO()
boardOnClick board p = boardOnPress board (\c -> liftIO (p c) >> return False)

boardOnPress :: Ix index => Board index tile piece -> ((index, index) -> EventM EButton Bool) -> IO()
boardOnPress board f = void $ do
  widgetAddEvents (boardDrawingArea board) [ButtonPressMask]
  (boardDrawingArea board) `on` buttonPressEvent $ mouseButtonHandler board f

boardOnRelease :: Ix index => Board index tile piece -> ((index, index) -> EventM EButton Bool) -> IO()
boardOnRelease board f = void $ do
  widgetAddEvents (boardDrawingArea board) [ButtonPressMask, ButtonReleaseMask]
  (boardDrawingArea board) `on` buttonReleaseEvent $ mouseButtonHandler board f

boardOnMotion :: Ix index => Board index tile piece -> ((index, index) -> EventM EMotion Bool) -> IO()
boardOnMotion board f =  void $ do
  widgetAddEvents board [PointerMotionMask]
  (boardDrawingArea board) `on` motionNotifyEvent $ mouseMotionHandler board f

boardSetBackground :: Ix index => Board index tile piece -> Maybe (Pixbuf, SizeAdjustment) -> IO()
boardSetBackground board bg = do
  writeIORef (background board) bg
  boardInvalidate board

boardSetOverlay :: Ix index => Board index tile piece -> Maybe (Pixbuf, SizeAdjustment) -> IO()
boardSetOverlay board bg = do
  writeIORef (overlay board) bg
  boardInvalidate board

boardEnableDrag :: Ix index => Board index tile piece -> IO()
boardEnableDrag board = writeIORef (dragEnabled board) True

boardDisableDrag :: Ix index => Board index tile piece -> IO()
boardDisableDrag board = do
  writeIORef (dragEnabled board) False
  boardInvalidate board

boardStartDrag :: Ix index => Board index tile piece -> (index, index) -> IO()
boardStartDrag board ix@(i,j) = do
  writeIORef (draggingFrom board) (Just ix)
  ((xm, ym), (xM, yM)) <- gameBoardGetBoundaries $ boardPieces board
  let (w,h) = tileSize board
      x     = round ((0.5 + fromIntegral (rangeSize (xm, i))) * fromIntegral w)
      y     = round ((0.5 + fromIntegral (rangeSize (ym, j))) * fromIntegral h)
  writeIORef (draggingMouseOrig board) (Just (x, y))

boardStopDrag :: Ix index => Board index tile piece -> IO()
boardStopDrag board = do
  writeIORef (draggingFrom board) Nothing
  writeIORef (draggingTo board) Nothing
  boardInvalidate board

boardOnPieceDragStart :: Ix index => Board index tile piece -> ((index, index) -> IO Bool) -> IO()
boardOnPieceDragStart board f = boardOnPress board $ \ix -> do
  (x,y) <- eventCoordinates
  returning False $ liftIO $ do
    drag <- readIORef (dragEnabled board)
    when drag $ do
      canDragThis <- f ix
      let from = if canDragThis then Just ix else Nothing
          orig = if canDragThis then Just (relativePos board ix (round x, round y)) else Nothing
      writeIORef (draggingFrom board) from
      writeIORef (draggingMouseOrig board) orig
      boardInvalidate board

boardOnPieceDragOver :: Ix index => Board index tile piece -> ((index, index) -> (index, index) -> IO Bool) -> IO()
boardOnPieceDragOver board f = boardOnMotion board $ \ix -> do
  (x,y) <- eventCoordinates
  returning False $ liftIO $ do
    drag  <- readIORef (dragEnabled board)
    origM <- readIORef (draggingFrom board)
    when (drag && isJust origM) $ do
      canDropHere <- f (fromJust origM) ix
      let newDest = if canDropHere then Just ix else Nothing
      writeIORef (draggingTo board) newDest
      writeIORef (draggingMousePos board) (Just (round x, round y))
    boardInvalidate board

boardOnPieceDragDrop :: Ix index => Board index tile piece -> ((index, index) -> (index, index) -> IO ()) -> IO()
boardOnPieceDragDrop board f = void $ do
  widgetAddEvents (boardDrawingArea board) [ButtonPressMask, ButtonReleaseMask]
  (boardDrawingArea board) `on` buttonReleaseEvent $ returning False $ liftIO $ do
    drag  <- readIORef (dragEnabled board)
    origM <- readIORef (draggingFrom board)
    destM <- readIORef (draggingTo board)
    let notSame = origM /= destM
    when (drag && isJust origM) $ do

      -- No longer dragging
      writeIORef (draggingFrom board)      Nothing
      writeIORef (draggingTo board)        Nothing
      writeIORef (draggingMouseOrig board) Nothing
      writeIORef (draggingMousePos board)  Nothing

      -- When possible, call the handler
      when (isJust destM && notSame) $ f (fromJust origM) (fromJust destM)

      -- In any case, the board must be repainted
      boardInvalidate board

boardIsDragging :: Ix index => Board index tile piece -> IO Bool
boardIsDragging = fmap isJust . readIORef . draggingFrom

relativePos :: Ix index => Board index tile piece -> (index, index) -> (Int, Int) -> (Int, Int)
relativePos board (ix,iy) (x,y) = (x', y')
 where (w,h) = tileSize board
       x'    = x `mod` w
       y'    = y `mod` h

returning :: Monad m => a -> m b -> m a
returning v f = f >> return v

-- boardLiveMove :: Ix index => (index, index) -> (index, index) -> Board index tile piece -> IO ()
-- boardLiveMove posO posD board = do
--   evs <- widgetGetEvents board
--   
--   let evs' = undefined
--   
--   movingParams <- undefined
-- 
--   let timeDelay = undefined
-- 
--   writeIORef (movingStatus board) (Just movingParams)
-- 
--   -- Set delay and time handler
--   timeoutAdd (do undefined -- advance moving one step
--                  boardInvalidate board
--                  -- return True if not the end, otherwise return False
--              )
--              timeDelay
--              
--      -- Time Handler includes resetting the event handers
--   return ()
