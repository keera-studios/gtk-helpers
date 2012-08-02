-- | Wraps a board widget with the correct size, pixbufs and auxiliary
-- functions to move pieces, load a new level and determine whether the game is
-- over.
module LightsOff
   ( LightsOffBoard
   , LightsOffLevel
   , LightsOffLevels
   , lightsOffBoardNew
   , lightsOffBoardSize
   , lightsOffBoardPositions
   , lightsOffBoardIsFinished
   , lightsOffBoardProcessMove
   , lightsOffBoardLoadLevel
   )
   
  where

import Data.Ix
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Board.TiledBoard

type LightsOffBoard = Board Int Tile Piece 

data Piece = Piece
data Tile = Tile

type LightsOffLevel  = [(Int,Int)]
type LightsOffLevels = [LightsOffLevel]

-- | Create a new board with the right tile and piece pixbufs
lightsOffBoardNew :: IO LightsOffBoard
lightsOffBoardNew = do

  tile   <- pixbufNewFromFile "player-piece-white.png"
  player <- pixbufNewFromFile "player-piece-black.png"

  let tileF   _ = tile
      playerF _ = player

  let allTiles = map (\(x,y) -> (x,y,Tile)) lightsOffBoardPositions
      
  boardNew allTiles tileF playerF

-- | Size of the game board
lightsOffBoardSize :: Int
lightsOffBoardSize = 5

-- | Return all the board positions
lightsOffBoardPositions :: [(Int, Int)]
lightsOffBoardPositions = [(x,y) | x <- [0..lightsOffBoardSize-1]
                                 , y <- [0..lightsOffBoardSize-1]]

-- | Toggle the color of a piece at a position and all those surrounding it
lightsOffBoardProcessMove :: Board Int tile Piece -> (Int, Int) -> IO ()
lightsOffBoardProcessMove board p = mapM_ (togglePiece board) $ surroundings p
  where surroundings (x,y) = [ (x', y')
                             | x' <- [x-1..x+1], y' <- [y-1..y+1]
                             , x == x' || y == y'
                             , inRange (0, lightsOffBoardSize-1) x'
                             , inRange (0, lightsOffBoardSize-1) y'
                             ]

-- | Determine whether the game is over
lightsOffBoardIsFinished :: Board Int tile Piece -> IO Bool
lightsOffBoardIsFinished board = boardFoldM board hasPiece True
  where hasPiece _ _ = return False

-- | Changes the color of the piece at a position
togglePiece :: Board Int tile Piece -> (Int, Int) -> IO ()
togglePiece board pos = do
  pieceM <- boardGetPiece pos board
  case pieceM of
   Nothing -> boardSetPiece pos Piece board
   Just _  -> boardRemovePiece pos board

-- | Load a new level
lightsOffBoardLoadLevel :: Board Int tile Piece -> LightsOffLevel -> IO ()
lightsOffBoardLoadLevel board level = boardLoad board $ map (\x -> (x,Piece)) level
