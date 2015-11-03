import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.IORef
import Data.Ix
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Board.TiledBoard

data Player = Player1 | Player2
 deriving (Eq)

data Tile = Tile

main :: IO () 
main = do
  -- View

  -- Initialise Gtk
  _ <- initGUI

  -- Create interface
  window  <- windowNew
  tile    <- pixbufNewFromFile "white-tile.png"
  player1 <- pixbufNewFromFile "player-piece-black.png"
  player2 <- pixbufNewFromFile "player-piece-white.png"

  let tileF _ = tile
      playerF Player1 = player1
      playerF Player2 = player2

  let allTiles = [(x,y,Tile) | x <- [0..2] :: [Int], y <- [0..2] :: [Int]]
  board <- boardNew allTiles tileF playerF
  mapM_ (\s -> widgetModifyBg board s (Color 50000 50000 65000)) [StateNormal, StateActive, StatePrelight, StateSelected]
  containerAdd window board

  -- Model

  -- Apart from the board, our internal model, used to determine moves, legal
  -- moves and the new board status
  nextPlayerRef <- newIORef Player1 -- Who plays now
  movingRef     <- newIORef Nothing -- Are we moving a piece?

  -- Controller
  board `boardOnClick` \pos -> do
    -- Whether a move is legal or not depends on
    nextPlayer <- readIORef nextPlayerRef    -- cur player
    movingM    <- readIORef movingRef        -- the piece we are moving, if any
    pieceM     <- boardGetPiece pos board    -- the piece on the position where the user clicked, if any
    num        <- numPieces board nextPlayer -- number of pieces that this player has already

    case (movingM, pieceM, num) of
     -- Add or move a piece, when legal
     (Nothing,   Nothing,    3) -> return ()
     (Nothing,   Nothing,    _) -> do boardSetPiece pos nextPlayer board
                                      modifyIORef nextPlayerRef togglePlayer
     (Just pos', Nothing,    _) -> do writeIORef movingRef Nothing
                                      boardMovePiece pos' pos board
                                      modifyIORef nextPlayerRef togglePlayer
     -- Start/cancel moving a piece
     (Nothing,   Just player', 3) -> when (nextPlayer == player') $ writeIORef movingRef (Just pos)
     (Just pos', Just _,       _) -> when (pos == pos') $ writeIORef movingRef Nothing

     -- Ignore clicking on a piece if we still have pieces to add
     _                                 -> return ()

  -- Close program if window is closed
  _ <- window `on` deleteEvent $ liftIO mainQuit >> return False

  -- Launch program with the main window
  widgetShowAll window
  mainGUI

numPieces :: Ix index => Board index tile Player -> Player -> IO Int
numPieces board player = boardFoldM board (\n (_,player') -> if player == player' then return (n + 1) else return n) 0

togglePlayer :: Player -> Player
togglePlayer Player1 = Player2 
togglePlayer Player2 = Player1

  -- set board [ widgetCanFocus := True ]
  -- board `on` keyPressEvent $ tryEvent $ do
  --   [Control] <- eventModifier
  --   "Return" <- eventKeyName
  --   liftIO $ putStrLn "Ctrl-Return pressed"

  -- vbox   <- vBoxNew False 2
  -- button <- buttonNewWithLabel "Press me"
  -- containerAdd vbox board
  -- containerAdd vbox button

  -- Reverse the text of the entry
  -- button `on` buttonActivated $ boardSetPiece (1,1) (Player1, Piece) board -- set entry [ entryText :~ reverse ]

-- data TicTacToeGame = GameBoard Int (Player, Piece)
-- 
-- ticTacToeFinished :: TicTacToeGame -> IO Bool
-- ticTacToeFinished board = return False
