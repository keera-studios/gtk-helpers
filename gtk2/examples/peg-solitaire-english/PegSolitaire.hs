{-# LANGUAGE MultiParamTypeClasses #-}
module PegSolitaire where

import Game.Board.BasicTurnGame
import Data.Ix
import Data.Maybe
import System.IO

checkmove :: Int -> Int -> Int -> Int -> Bool
checkmove x1 y1 x2 y2 =   
  if (abs (x1 - x2 ) == 0 && abs (y1 -y2 ) ==2 ) 
  then True
  else if ( abs (x1 - x2 ) == 2 && abs (y1 -y2 ) ==0 )    
  then True
  else False

midpoint :: Int -> Int -> Int -> Int -> (Int,Int)
midpoint x1 y1 x2 y2 = (  ( div  (x1+x2) 2 ), (div(y1+y2) 2 ))


data Peg    = Peg
data Tile   = Tile
data Player = Player

newtype PegSolitaireGame = PegSolitaireGame (GameState Int Tile Player Peg)


-- Basic game definition
defaultPegSolitaireGame :: PegSolitaireGame
defaultPegSolitaireGame = PegSolitaireGame $ GameState
 { curPlayer'      = Player 
 , boardPos        = allTiles
 , boardPieces'    = pieces
 }
 where allTiles =[(x,y,Tile) |x <- [-3..3] :: [Int], y <- [-3..3] :: [Int], not (( x < -1 || x > 1 ) && ( y > 1 || y < -1 )) ]
       
       pieces   = [(x,y,Player,Peg) |(x,y,_) <- allTiles, not ( (x==0) && (y==0) )]

instance PlayableGame PegSolitaireGame Int Tile Player Peg where

  -- "Static" game view
  curPlayer (PegSolitaireGame game) = curPlayer' game
  allPieces (PegSolitaireGame game) = boardPieces' game
  allPos (PegSolitaireGame game) = boardPos game

  -- Kind of moves that are allowed
  moveEnabled _     = True
  canMove _ _ _     = True
  canMoveTo _ _ _ _ = True

  -- Convert a "move" to a sequence of changes
  
  move (PegSolitaireGame game) _player posO posD
    | hasPiece game posO && hasPiece game posI && not (hasPiece game posD) && checkmove (fst posO) (snd posO) (fst posD) (snd posD)
    = [ MovePiece posO posD, RemovePiece posI ]
    | otherwise
    = []
   where posI   =    midpoint (fst posO) (snd posO) (fst posD) (snd posD)

  -- Apply a change to the game
  applyChange psg@(PegSolitaireGame game) (MovePiece posO posD)
    | Just (player, piece) <- getPieceAt game posO
    = applyChanges psg [RemovePiece posO, RemovePiece posD, AddPiece posD player piece]

--    | otherwise = psg
  applyChange (PegSolitaireGame game) (AddPiece (x,y) player piece )
    = PegSolitaireGame (game { boardPieces' = (x,y,player,piece) : boardPieces' game })
  applyChange (PegSolitaireGame game) (RemovePiece (x,y))
    = PegSolitaireGame (game { boardPieces' = [ (x',y',player,piece)
                                              | (x',y',player,piece) <- boardPieces' game
                                              , (x /= x' || y /= y')]})
