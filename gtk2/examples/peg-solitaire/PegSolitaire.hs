{-# LANGUAGE MultiParamTypeClasses #-}
module PegSolitaire where

import Game.Board.BasicTurnGame

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
 where allTiles = [(x,y,Tile) | x <- [0..6] :: [Int], y <- [0..6] :: [Int], not (inCorner x y)]
       inCorner x y =  ((x > 4 || x < 2) && (y == 0 || y == 6))
                    || ((y > 4 || y < 2) && (x == 0 || x == 6))
       pieces   = [(x,y,Player,Peg) | (x,y,_) <- allTiles, (x /= 3 || y /= 3)]

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
    | hasPiece game posO && hasPiece game posI && not (hasPiece game posD) && correctDiff
    = [ MovePiece posO posD, RemovePiece posI ]
    | otherwise
    = []
   where diffX = abs (fst posO - fst posD)
         diffY = abs (snd posD - snd posO)
         correctDiff = (diffX == 0 && diffY == 2) || (diffX == 2 && diffY == 0)
         posI        = ((fst posO + fst posD) `div` 2, (snd posO + snd posD) `div` 2)

  -- Apply a change to the game
  applyChange psg@(PegSolitaireGame game) (MovePiece posO posD)
    | Just (player, piece) <- getPieceAt game posO
    = applyChanges psg [RemovePiece posO, RemovePiece posD, AddPiece posD player piece]

    | otherwise = psg
  applyChange (PegSolitaireGame game) (AddPiece (x,y) player piece )
    = PegSolitaireGame (game { boardPieces' = (x,y,player,piece) : boardPieces' game })
  applyChange (PegSolitaireGame game) (RemovePiece (x,y))
    = PegSolitaireGame (game { boardPieces' = [ (x',y',player,piece)
                                              | (x',y',player,piece) <- boardPieces' game
                                              , (x /= x' || y /= y')]})
