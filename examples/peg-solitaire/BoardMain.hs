import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.Maybe
import Data.IORef
import Data.Ix
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Board.TiledBoard
import Graphics.UI.Gtk.Layout.BackgroundContainer

data Peg  = Peg
data Tile = Tile

main :: IO () 
main = do
  -- View

  -- Initialise Gtk
  _ <- initGUI

  -- Create interface
  window  <- windowNew
  vbox    <- vBoxNew False 3
  bgBin   <- backgroundContainerNewWithPicture "Free-Background-3.jpg"
  align   <- alignmentNew 0.5 0.5 0 0
  lbl     <- labelNew Nothing
  btn     <- buttonNewWithLabel "Restart Level"

  -- The images used for tiles and pegs
  tile    <- pixbufNewFromFile "player-piece-white.png"
  pegPb   <- pixbufNewFromFile "player-piece-black.png"

  -- Assign the proper pixbufs to tiles and player pieces
  let tileF _ = tile
      pegF  _ = pegPb

  let allTiles = [(x,y,Tile) | x <- [0..6] :: [Int], y <- [0..6] :: [Int], not (inCorner x y)]
      inCorner x y =  ((x > 4 || x < 2) && (y == 0 || y == 6))
                   || ((y > 4 || y < 2) && (x == 0 || x == 6))
      pieces   = [(x,y) | (x,y,_) <- allTiles, (x /= 3 || y /= 3)]

  board <- boardNew allTiles tileF pegF

  pb <- pixbufNewFromFile "woodciircle.1.png"
  boardSetBackground board (Just (pb, SizeAdjustment))

  containerAdd align board
  containerAdd bgBin align
  boxPackStart vbox bgBin    PackGrow 0
  boxPackStart vbox lbl      PackNatural 1
  boxPackStart vbox btn      PackNatural 2
  containerAdd window vbox

  widgetSetSizeRequest window 400 300

  -- Set the initial board state
  mapM_ (\x -> boardSetPiece x Peg board) pieces

  -- Properties and handlers of the board
  board `boardOnPieceDragStart` attemptDragStart board
  board `boardOnPieceDragOver`  attemptDragOver board
  board `boardOnPieceDragDrop`  attemptMove board
  boardEnableDrag board

  -- Close program if window is closed
  _ <- window `on` deleteEvent $ liftIO mainQuit >> return False

  -- Launch program with the main window
  widgetShowAll window
  mainGUI

-- We do not need to give any feedback, so dragging is always allowed
attemptDragStart :: Board Int Tile Peg -> (Int, Int) -> IO Bool
attemptDragStart _ _ = return True

-- We do not need to give any feedback, so dragging is always allowed
attemptDragOver :: Board Int Tile Peg -> (Int, Int) -> (Int, Int) -> IO Bool
attemptDragOver _ _ _ = return True

attemptMove :: Board Int Tile Peg -> (Int, Int) -> (Int, Int) -> IO ()
attemptMove board p1 p2 = do
  pieceM     <- boardGetPiece p2 board           -- the piece on the position where the user dropped, if any
  interPiece <- boardGetPiece intermediate board -- the piece between the one we are moving and the hole
  when (correctDiff && isJust interPiece && isNothing pieceM) $ do
    boardMovePiece p1 p2 board
    boardRemovePiece intermediate board
  where diffX = abs (fst p1 - fst p2)
        diffY = abs (snd p1 - snd p2)
        correctDiff  = (diffX == 0 && diffY == 2) || (diffX == 2 && diffY == 0)
        intermediate = ((fst p1 + fst p2) `div` 2, (snd p1 + snd p2) `div` 2)
