import Data.List (intercalate, find, nub)
import Data.Maybe (isJust, isNothing)
import System.IO (hSetBuffering, BufferMode(NoBuffering, LineBuffering), stdin, stdout)
import Data.Word (Word)
import Data.Char (digitToInt)
import Control.Monad.Trans.State.Strict (StateT, get, modify', execStateT)
import Control.Monad.IO.Class (liftIO)

data Player = X | O
    deriving (Eq, Show)

data Triple a = Triple a a a

toList :: Triple a -> [a]
toList (Triple a b c) = [a,b,c]

instance Functor Triple where
    fmap f (Triple a b c) = Triple (f a) (f b) (f c)

type Board = Triple (Triple (Maybe Player))
type Index = (Word, Word)

data GameState = Winner Player | Unfinished | Tie

initialBoard :: Board
initialBoard = let row = Triple Nothing Nothing Nothing in Triple row row row

allFilled :: Eq a => Triple (Triple (Maybe a)) -> Bool
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

--all the rows/columns/diagonals in which to check for a winner
triplets :: [Triple Index]
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
printBoard = putStrLn . intercalate "\n" . toList . fmap (toList . printRow)
    where printSlot (Just X) = 'X'
          printSlot (Just O) = 'O'
          printSlot Nothing  = 'E'
          printRow  = fmap printSlot

getItem :: Word -> Triple a -> a
getItem 0 (Triple a _ _) = a
getItem 1 (Triple _ b _) = b
getItem 2 (Triple _ _ c) = c
getItem n _ = error $ "This cannot happen: getItem " ++ show n

index :: Index -> Triple (Triple a) -> a
index (r,c) = getItem c . getItem r

place :: Index -> Player -> Board -> Board
place (row,col) player board = placeRow (placeRow (Just player) col (getItem row board)) row board
    where placeRow x 0 (Triple _ b c) = Triple x b c
          placeRow x 1 (Triple a _ c) = Triple a x c
          placeRow x 2 (Triple a b _) = Triple a b x
          placeRow _ n _ = error $ "This cannot happen: placeRow " ++ show n

analyze :: Board -> GameState
analyze board = case winner board of
    Just player -> Winner player
    Nothing -> if allFilled board then Tie else Unfinished

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
    modify' $ place (suggestMove O board) O where

    suggestMove player board = case winningPositions player board of
        x:_ -> x
        [] -> case winningPositions (otherPlayer player) board of
            x:_ -> x
            --Go in the middle whenever you can. This results in a guaranteed tie unless the
            --other person messes up.
            --
            --Using head is safe here because if the board was full then we wouldn't
            --have called this function.
            [] ->  if isNothing $ index (1,1) board then (1,1) else head $ openSpots board
        where
            otherPlayer X = O
            otherPlayer O = X

    winningPositions player board = map fst . filter f . map (\x -> (x, place x player board)) . openSpots $ board where
        f (_, board') = case analyze board' of
            Winner _ -> True
            _ -> False

    allSpots = nub $ concatMap toList triplets

    openSpots board = filter (\x -> isNothing $ index x board) allSpots

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
