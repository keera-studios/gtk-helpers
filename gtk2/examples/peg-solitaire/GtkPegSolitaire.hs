{-# LANGUAGE MultiParamTypeClasses #-}

module GtkPegSolitaire where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Board.TiledBoard
import Graphics.UI.Gtk.Board.BoardLink
import PegSolitaire

gtkGame :: IO (Game PegSolitaireGame Int Tile Player Peg)
gtkGame = do
  -- The images used for tiles and pegs
  tile  <- pixbufNewFromFile "player-piece-white.png"
  pegPb <- pixbufNewFromFile "player-piece-black.png"
  pb    <- pixbufNewFromFile "woodciircle.1.png"

  let game = Game visualAspects defaultPegSolitaireGame
      visualAspects = VisualGameAspects { tileF   = \_ -> tile
                                        , pieceF  = \_ -> pegPb
                                        , bgColor = (65000, 50000, 50000)
                                        , bg      = Just (pb, SizeAdjustment)
                                        }

  return game
