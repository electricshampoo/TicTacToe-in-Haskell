import Data.List (intercalate, find)
import Data.Maybe (isJust)
import System.IO
import Data.Word
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class

data Player = X | O
    deriving (Eq, Show)
data Triple a = Triple a a a

instance Functor Triple where
    fmap f (Triple a b c) = Triple (f a) (f b) (f c)

type Board = Triple (Triple (Maybe Player))

data GameState = Winner Player | Unfinished | Tie

initialBoard :: Board
initialBoard = let row = Triple Nothing Nothing Nothing in Triple row row row

allFilled :: Board -> Bool
allFilled (Triple r1 r2 r3) = allFilled' r1 && allFilled' r2 && allFilled' r3
    where allFilled' (Triple (Just _) (Just _) (Just _)) = True
          allFilled' _ = False

allSame :: Eq a => Triple (Maybe a) -> Maybe a
allSame (Triple (Just a) (Just b) (Just c)) = if a == b && b == c then Just c else Nothing
allSame _ = Nothing

winner :: Board -> Maybe Player
winner board = join . find isJust . map allSame $ map (fmap $ flip index board) triplets
    where join (Just a) = a
          join Nothing = Nothing

triplets :: [Triple (Word,Word)]
triplets = [Triple (0,0) (0,1) (0,2)
           ,Triple (1,0) (1,1) (1,2)
           ,Triple (2,0) (2,1) (2,2)
           ,Triple (0,0) (1,0) (2,0)
           ,Triple (0,1) (1,1) (2,1)
           ,Triple (0,2) (1,2) (2,2)
           ,Triple (0,0) (1,1) (2,2)
           ,Triple (0,2) (1,1) (2,0)
           ]

printBoard :: Board -> IO ()
printBoard = putStrLn . intercalate "\n" . map toList . toList . fmap printRow
    where printSlot (Just X) = 'X'
          printSlot (Just O) = 'O'
          printSlot Nothing  = 'E'
          printRow  = fmap printSlot
          toList (Triple a b c) = [a,b,c]

getItem :: Word -> Triple a -> a
getItem 0 (Triple a _ _) = a
getItem 1 (Triple _ b _) = b
getItem 2 (Triple _ _ c) = c

index :: (Word, Word) -> Board -> Maybe Player
index (r,c) = getItem c . getItem r

place :: (Word, Word) -> Player -> Board -> Board
place (row,col) player board = placeRow (placeRow (Just player) col (getItem row board)) row board
    where
        placeRow x 0 (Triple _ b c) = Triple x b c
        placeRow x 1 (Triple a _ c) = Triple a x c
        placeRow x 2 (Triple a b _) = Triple a b x

analyze :: Board -> GameState
analyze board = case winner board of
    Just player -> Winner player
    Nothing -> if allFilled board then Tie else Unfinished

getPlayerInput :: Player -> StateT Board IO ()
getPlayerInput player = do
    board <- get
    liftIO $ printBoard board
    liftIO $ putStr $ "Player " ++ show player ++ " enter position: "
    line <- liftIO getLine
    let r = (read line !! 0)
        c = (read line !! 2)
    --TODO: check if already taken
    modify' (place (r,c) player)

runGameIteration :: StateT Board IO ()
runGameIteration = do
    getPlayerInput X
    board <- get
    case analyze board of
        Winner _ -> liftIO $ putStrLn "Congrats player X. You won!"
        Tie -> liftIO $ putStrLn "The game was a tie."
        Unfinished -> do
            getPlayerInput O
            board' <- get
            case analyze board' of 
                Winner _ -> liftIO $ putStrLn "Congrats player O. You won!"
                Tie -> liftIO $ putStrLn "The game was a tie."
                Unfinished -> runGameIteration

main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout NoBuffering
    board <- execStateT runGameIteration initialBoard 
    printBoard board
