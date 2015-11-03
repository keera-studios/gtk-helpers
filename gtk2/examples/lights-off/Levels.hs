-- | List of lightsoff levels. Each level is a list of pairs of positions. Some
-- are defined giving all the positions, others by constraining the list with
-- all board positions.
module Levels where

import LightsOff

levels :: [LightsOffLevel]
levels =
   [ [(x,y) | x <- [c1-1..c1+1], y <- [c2-1..c2+1], x == c1 || y == c2]
   , filter (\(x,y) -> (x /= 1 || y /= 1)) lightsOffBoardPositions
   , filter (\(x,y) -> (x == y || x == (lightsOffBoardSize - 1 - y))) lightsOffBoardPositions
   , filter (\(x,y) -> (x == y)) lightsOffBoardPositions
   , ( centre : [(x,y) | x <- [c1-1, c1+1] , y <- [c2-1, c2+1]])
   , [(1,1)]
   , [(0,1), (1,1), (2,1)]
   , [centre]
   , [(2,1), (2,3)]
   , [(0,0)]
   , [ head lightsOffBoardPositions, last lightsOffBoardPositions ]
   ]
 where centre@(c1, c2) = let p = lightsOffBoardSize `div` 2 in (p,p)
