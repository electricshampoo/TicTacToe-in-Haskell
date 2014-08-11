import Data.Maybe (isJust)
import System.IO (hSetBuffering, BufferMode(NoBuffering, LineBuffering), stdin, stdout)
import Data.Word (Word)
import Data.Char (digitToInt)
import Control.Monad.Trans.State.Strict (StateT, get, modify', execStateT)
import Control.Monad.IO.Class (liftIO)
import Board
import AI

getPlayerInput :: Board -> Player -> IO Index
getPlayerInput board player = do
    printBoard board
    putStr $ "Player " ++ show player ++ " enter position (row column): "
    line <- getLine

    let r = fromIntegral . digitToInt $ line !! 0 :: Word
        c = fromIntegral . digitToInt $ line !! 2 :: Word

    if r > 2 || c > 2
    then do
        putStrLn $ "Indicies have to be in the range [0,2]"
        getPlayerInput board player
    else if isJust $ index (r,c) board
         then do
             putStrLn $ "This spot is already taken. Try again."
             getPlayerInput board player
         else return $! (r,c)

placePlayer :: Player -> StateT Board IO ()
placePlayer X = do
    board <- get
    (r,c) <- liftIO $ getPlayerInput board X
    modify' $ place (r,c) X
placePlayer O = do
    board <- get
    modify' $ place (suggestMove O board) O

runGame :: StateT Board IO ()
runGame = do
    placePlayer X
    board <- get
    case analyze board of
        Winner _ -> liftIO $ putStrLn "Congrats player X. You won!"
        Tie -> liftIO $ putStrLn "The game was a tie."
        Unfinished -> do
            placePlayer O
            board' <- get
            case analyze board' of
                Winner _ -> liftIO $ putStrLn "The computer won."
                Tie -> liftIO $ putStrLn "The game was a tie."
                Unfinished -> runGame

main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout NoBuffering
    board <- execStateT runGame initialBoard
    printBoard board
