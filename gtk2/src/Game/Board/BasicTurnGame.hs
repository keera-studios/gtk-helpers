{-# Language MultiParamTypeClasses, FunctionalDependencies #-}

module Game.Board.BasicTurnGame where

import Data.Ix
import Data.Maybe

data GameChange index player piece 
  = AddPiece (index, index) player piece
  | RemovePiece (index, index)
  | MovePiece (index, index) (index, index)

-- FIXME: Wrong data structure => use unmutable matrices
hasPiece :: Ix index => GameState index tile player piece -> (index, index) -> Bool
hasPiece game ix = isJust (getPieceAt game ix)

getPieceAt :: Ix index => GameState index tile player piece -> (index, index) -> Maybe (player, piece)
getPieceAt game (posX, posY) = 
  listToMaybe [(player, piece) | (x, y, player, piece) <- boardPieces' game
                               , x == posX
                               , y == posY]
  

data GameState index tile player piece = GameState
  { curPlayer'   :: player
  , boardPos     :: [(index, index, tile)]
  , boardPieces' :: [(index, index, player, piece)]
  }

class PlayableGame a index tile player piece | a -> index, a -> tile, a -> player, a -> piece where

  curPlayer :: a -> player
  allPieces :: a -> [(index, index, player, piece)]
  allPos    :: a -> [(index, index, tile)]

  moveEnabled     :: a -> Bool
  moveEnabled _ = False

  canMove         :: a -> player -> (index, index) -> Bool
  canMove _ _ _ = False

  canMoveTo       :: a -> player -> (index, index) -> (index, index) -> Bool
  canMoveTo _ _ _ _ = False

  move            :: a -> player -> (index, index) -> (index, index) -> [GameChange index player piece]
  move _ _ _ _ = []

  activateEnabled :: a -> Bool
  activateEnabled _ = False

  canActivate     :: a -> player -> (index, index) -> Bool
  canActivate _ _ _ = False

  activate        :: a -> player -> (index, index) -> [GameChange index player piece]
  activate _ _ _ = []

  applyChange     :: a -> GameChange index player piece -> a
  applyChange g _ = g

  applyChanges    :: a -> [GameChange index player piece] -> a
  applyChanges a ls = foldl applyChange a ls
