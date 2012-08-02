-- | Message pool
module Strings where

data StringId = StrClickToPlay
              | StrWonGame
              | StrWonAllGames
              | StrNothing

strings :: StringId -> String
strings StrClickToPlay = "Click the board to start the game to play"
strings StrWonGame     = "You won! Click the board to start the next game"
strings StrWonAllGames = "You won! You must be really awesome :)"
strings StrNothing     = ""
