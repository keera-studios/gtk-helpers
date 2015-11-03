{-# Language MultiParamTypeClasses, FunctionalDependencies #-}

module Graphics.UI.Gtk.Board.BoardLink where

import Control.Monad
import Data.IORef
import Data.Ix
import Data.Maybe
import Game.Board.BasicTurnGame
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Board.TiledBoard


attachGameRules :: (PlayableGame pg index tile player piece, Ix index)
                => Game pg index tile player piece -> IO (Board index tile (player, piece))
attachGameRules game = do
  board <- boardNew (allPos $ gameS game) (tileF $ visual game) (pieceF $ visual game)

  let (r,g,b) = bgColor (visual game)
      (r', g', b') = (fromIntegral r, fromIntegral g, fromIntegral b)
  mapM_ (\s -> widgetModifyBg board s (Color r' g' b')) [StateNormal, StateActive, StatePrelight, StateSelected]
  when (isJust (bg $ visual game)) $ boardSetBackground board (bg $ visual game)

  vgRef <- newIORef game

  -- Set the initial board state
  mapM_ (\(x,y) -> boardSetPiece x y board) $ [((x,y),(pl,pc)) | (x,y,pl,pc) <- allPieces (gameS game)]

  board `boardOnPieceDragStart` \pos -> do
    visualGame <- readIORef vgRef
    let game' = gameS visualGame
    return (moveEnabled game' && canMove game' (curPlayer game') pos)

  board `boardOnPieceDragOver` \posF posT -> do
    visualGame <- readIORef vgRef
    let game' = gameS visualGame
    return (moveEnabled game' && canMoveTo game' (curPlayer game') posF posT)

  board `boardOnPieceDragDrop` \posF posT -> do
    visualGame <- readIORef vgRef
    let game'  = gameS visualGame
        moves  = move game' (curPlayer game') posF posT
        game'' = foldl applyChange game' moves
    writeIORef vgRef (visualGame { gameS = game'' })
    forM_ moves (applyBoardChange board)
    
  when (moveEnabled (gameS game)) $ boardEnableDrag board

  return board

applyBoardChange :: Ix index => Board index tile (player, piece) -> GameChange index player piece -> IO ()
applyBoardChange board (AddPiece pos player piece) = boardSetPiece pos (player, piece) board
applyBoardChange board (RemovePiece pos)           = boardRemovePiece pos board
applyBoardChange board (MovePiece posO posD)       = boardMovePiece posO posD board

data VisualGameAspects index tile player piece = VisualGameAspects
  { tileF   :: PixmapsFor tile
  , pieceF  :: PixmapsFor (player, piece)
  , bgColor :: (Int, Int, Int)
  , bg      :: Maybe (Pixbuf, SizeAdjustment)
  } 


data Game pg index tile player piece = Game
  { visual :: VisualGameAspects index tile player piece
  , gameS  :: pg
  }
