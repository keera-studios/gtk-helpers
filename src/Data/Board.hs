module Data.Board where

import Control.Monad
import Data.Array.IO
import Data.Maybe

data GameBoard index e = GameBoard (IOArray (index,index) (Maybe e))

gameBoardNew :: (Ix index) => [(index, index, e)] -> IO (GameBoard index e)
gameBoardNew es = do
    array <- newArray (listBoundaries indexList) Nothing
    mapM_ (\(i,j,e) -> writeArray array (i,j) (Just e)) es
    return $ GameBoard array
  where indexList = map (\(i,j,_) -> (i,j)) es

gameBoardNewEmptySquare :: (Num index, Ix index) => index -> index -> IO (GameBoard index e)
gameBoardNewEmptySquare iX jX = do
    array <- newArray ((0,0),(iX,jX)) Nothing
    return $ GameBoard array

gameBoardNewEmpty :: Ix index => [(index, index)] -> IO (GameBoard index e)
gameBoardNewEmpty es = do
    array <- newArray (listBoundaries es) Nothing
    return $ GameBoard array

listBoundaries :: Ix index => [(index,index)] -> ((index,index),(index,index))
listBoundaries ((ix,jx):es) = foldr updateBoundaries ((ix,jx),(ix,jx)) es
  where updateBoundaries (x,y) ((minX,minY),(maxX,maxY)) = ((minX',minY'),(maxX',maxY'))
         where minX' = if x < minX then x else minX
               minY' = if y < minY then y else minY
               maxX' = if x > maxX then x else maxX
               maxY' = if y > maxY then y else maxY

gameBoardSetPiece :: Ix index => (index, index) -> e -> GameBoard index e -> IO () -- GameBoard index e)
gameBoardSetPiece pos e (GameBoard board) =
  writeArray board pos (Just e)

gameBoardGetPiece :: Ix index => (index, index) -> GameBoard index e -> IO (Maybe e)
gameBoardGetPiece pos (GameBoard board) =
  readArray board pos

gameBoardRemovePiece :: Ix index => (index, index) -> GameBoard index e -> IO () -- GameBoard index e)
gameBoardRemovePiece pos (GameBoard board) =
  writeArray board pos Nothing

gameBoardMovePiece :: Ix index => (index, index) -> (index, index) -> GameBoard index e -> IO () -- GameBoard index e)
gameBoardMovePiece posO posD gb = do
  piece <- gameBoardGetPiece posO gb
  maybe (return ()) (\piece' -> do
    gameBoardRemovePiece posO gb
    gameBoardSetPiece posD piece' gb) piece

gameBoardFoldM :: (Ix index) => GameBoard index a -> (b -> ((index,index), a) -> IO b) -> b -> IO b
gameBoardFoldM (GameBoard array) f def = do
  assocs <- getAssocs array
  let assocs' = map (\(x,y) -> (x,fromJust y)) $ filter (isJust . snd) assocs
  foldM f def assocs'

gameBoardFoldM_ :: (Ix index) => GameBoard index a -> ((index,index) -> a -> IO ()) -> IO ()
gameBoardFoldM_ (GameBoard array) f =
  foldArrayM_ array f'
 where f' x e = maybe (return ()) (f x) e

foldArrayM_ :: (Ix index) => IOArray index a -> (index -> a -> IO ()) -> IO ()
foldArrayM_ array f = do
  assocs <- getAssocs array
  mapM_ (uncurry f) assocs
