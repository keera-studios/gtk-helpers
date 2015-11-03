import Control.Monad
import Control.Monad.IfElse
import Control.Monad.Trans (liftIO)
import Data.Maybe
import Data.IORef
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Board.TiledBoard
import Graphics.UI.Gtk.Layout.BackgroundContainer

import Levels
import LightsOff
import Strings

main :: IO () 
main = do
  -- View

  -- Initialise Gtk
  _ <- initGUI

  -- Create interface
  window   <- windowNew
  vbox     <- vBoxNew False 3
  bgBin    <- backgroundContainerNewWithPicture "bg8large.jpg"
  align    <- alignmentNew 0.5 0.5 0 0
  board    <- lightsOffBoardNew
  lbl      <- labelNew (Just (strings StrClickToPlay))
  btn      <- buttonNewWithLabel "Restart Level"

  pb <- pixbufNewFromFile "overlay.png"

  boardSetOverlay board (Just (pb, SizeAdjustment))

  containerAdd align board
  containerAdd bgBin align
  boxPackStart vbox bgBin    PackGrow 0
  boxPackStart vbox lbl      PackNatural 1
  boxPackStart vbox btn      PackNatural 2
  containerAdd window vbox
  widgetSetSizeRequest window 400 300

  -- Model
  modelRef <- newIORef (Nothing, levels)

  -- Environment
  let env = (UI lbl board, Model modelRef)

  -- Controller
  board `boardOnRelease` (attemptMove env)
  _ <- btn `onClicked` (reloadGame env)

  -- Close program if window is closed
  _ <- window `on` deleteEvent $ liftIO mainQuit >> return False

  -- Launch program with the main window
  widgetShowAll window
  mainGUI

type Env   = (UI, Model)
data UI    = UI { uiLabel :: Label, uiBoard :: LightsOffBoard }
data Model = Model { modRef :: IORef (Maybe LightsOffLevel, LightsOffLevels) }

attemptMove :: Env -> (Int, Int) -> EventM EButton Bool
attemptMove env@(ui, model) p = do
  btn <- eventButton
  when (btn == LeftButton) $ liftIO $ do
    let board = uiBoard ui

    gameOn <- fmap (isJust . fst) $ readIORef (modRef model)
    remainingLevels <- fmap snd $ readIORef (modRef model)

    -- Process the move or load the next level
    if gameOn
     then do lightsOffBoardProcessMove board p
             finished <- lightsOffBoardIsFinished board
             when finished $ do
               let msg = if null remainingLevels then StrWonAllGames else StrWonGame
               labelSetText (uiLabel ui) (strings msg)
               writeIORef (modRef model) (Nothing, remainingLevels)
     else do advanceGame env
             reloadGame env
             labelSetText (uiLabel ui) (strings StrNothing)
  return False

-- Moves to the next level, if there is one
advanceGame :: Env -> IO ()
advanceGame (_, model) = modifyIORef (modRef model) nextGame
 where nextGame v@(_, [])   = v
       nextGame   (_, l:ls) = (Just l, ls)

-- Reloads the current level if there is one
reloadGame :: Env -> IO ()
reloadGame (ui, model) =
  awhenM (fmap fst $ readIORef (modRef model)) $
    lightsOffBoardLoadLevel (uiBoard ui)
