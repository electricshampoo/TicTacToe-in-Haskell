module Board where

import Data.List (intercalate, find, nub)
import Data.Maybe (isJust, isNothing)
import Data.Word (Word)

data Player = X | O
    deriving (Eq, Show)

data Triple a = Triple a a a
    deriving (Show, Eq)

toList :: Triple a -> [a]
toList (Triple a b c) = [a,b,c]

instance Functor Triple where
    fmap f (Triple a b c) = Triple (f a) (f b) (f c)

type Board = Triple (Triple (Maybe Player))
type Index = (Word, Word)

data GameState = Winner Player | Unfinished | Tie
    deriving (Eq)

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

allSpots :: [Index]
allSpots = nub $ concatMap toList triplets

openSpots :: Board -> [Index]
openSpots board = filter (\x -> isNothing $ index x board) allSpots

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
