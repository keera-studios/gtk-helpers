{-# Language MultiParamTypeClasses, FunctionalDependencies #-}

import Control.Monad.Trans (liftIO)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Layout.BackgroundContainer
import Graphics.UI.Gtk.Board.BoardLink
import GtkPegSolitaire

main :: IO ()
main = do
  -- View

  -- Initialise Gtk
  _ <- initGUI

  -- Create interface
  window  <- windowNew
  -- vbox    <- vBoxNew False 2
  bgBin   <- backgroundContainerNewWithPicture "Free-Background-3.jpg"
  align   <- alignmentNew 0.5 0.5 0 0

  game    <- gtkGame
  board   <- attachGameRules game

  containerAdd align board
  containerAdd bgBin align
  containerAdd window bgBin

  widgetSetSizeRequest window 400 300

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
