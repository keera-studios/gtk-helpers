import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.Maybe
import Data.IORef
import Data.Ix
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Board.TiledBoard

data Player = Player
 deriving (Eq, Show)

data Piece = Piece
 deriving (Eq, Show)

data Tile = Tile

main :: IO () 
main = do
  -- View

  -- Initialise Gtk
  _ <- initGUI

  -- Create interface
  window  <- windowNew
  tile    <- pixbufNewFromFile "player-piece-white.png"
  player1 <- pixbufNewFromFile "player-piece-black.png"

  let tileF   _ = tile
      playerF _ = player1

  let allTiles     = [(x,y,Tile) | x <- [0..6] :: [Int], y <- [0..6] :: [Int], not (inCorner x y)]
      inCorner x y = (x > 4 || x < 2) && (y < 2 || y > 4)
      pieces       = [(x,y) | (x,y,_) <- allTiles, (x /= 3 || y /= 3)]
      
  board <- boardNew allTiles tileF playerF (32,32)
  mapM_ (\x -> boardSetPiece x (Player,Piece) board) pieces
  containerAdd window board

  -- Model
  --
  -- Apart from the board, our internal model, used to determine moves, legal
  -- moves and the new board status
  movingRef <- newIORef Nothing -- Are we moving a piece?

  -- Controller
  board `boardOnClick` \pos -> do
    movingM <- readIORef movingRef -- the pos of the piece we are moving, if any
    case movingM of
      Just pos' -> do writeIORef movingRef Nothing
                      attemptMove board pos' pos
      Nothing   -> writeIORef movingRef . fmap (const pos) =<< boardGetPiece pos board

  -- Close program if window is closed
  _ <- window `on` deleteEvent $ liftIO mainQuit >> return False

  -- Launch program with the main window
  widgetShowAll window
  mainGUI

attemptMove :: Board Int Player tile Piece -> (Int, Int) -> (Int, Int) -> IO ()
attemptMove board p1 p2 = do
  pieceM     <- boardGetPiece p2 board           -- the piece on the position where the user clicked, if any
  interPiece <- boardGetPiece intermediate board -- the piece between the one we are moving and the hole
  when (correctDiff && isJust interPiece && isNothing pieceM) $ do
    boardMovePiece p1 p2 board
    boardRemovePiece intermediate board
  where diffX = abs (fst p1 - fst p2)
        diffY = abs (snd p1 - snd p2)
        correctDiff  = (diffX == 0 && diffY == 2) || (diffX == 2 && diffY == 0)
        intermediate = ((fst p1 + fst p2) `div` 2, (snd p1 + snd p2) `div` 2)
