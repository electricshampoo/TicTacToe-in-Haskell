module AI where

import Board
import Data.Maybe (isNothing)

suggestMove :: Player -> Board -> Index
suggestMove player board = case winningPositions player board of
    x:_ -> x
    [] -> case winningPositions (otherPlayer player) board of
        x:_ -> x
        --Go in the middle whenever you can. Using head is safe here because if the board was full then we wouldn't
        --have called this function.
        [] ->  case lookup board specialCases of
            Just x -> x
            Nothing -> if isNothing $ index (1,1) board then (1,1) else head $ openSpots board
    where
        otherPlayer X = O
        otherPlayer O = X

        winningPositions player' board' = filter (\ix -> (== Winner player') . analyze $ place ix player' board')
                                        . openSpots $ board'

        --assumes AI is player O
        specialCases = [(Triple (Triple (Just X) (Just O) Nothing)
                                (Triple Nothing  (Just O) (Just X))
                                (Triple Nothing  (Just X) Nothing), (2,0))

                       ,(Triple (Triple (Just X) Nothing Nothing)
                                (Triple Nothing  (Just O) Nothing)
                                (Triple Nothing  (Just X) Nothing), (2,0))

                       ,(Triple (Triple Nothing  Nothing (Just X))
                                (Triple Nothing  (Just O) Nothing)
                                (Triple (Just X) Nothing  Nothing), (1,0))

                       ,(Triple (Triple Nothing  Nothing  (Just X))
                                (Triple Nothing  (Just O) Nothing)
                                (Triple Nothing  (Just X) Nothing), (1,0))

                       ,(Triple (Triple (Just O) Nothing  Nothing)
                                (Triple Nothing  (Just X) Nothing)
                                (Triple Nothing  Nothing (Just X)), (0,2))

                       ,(Triple (Triple Nothing Nothing  Nothing)
                                (Triple Nothing (Just O) (Just X))
                                (Triple (Just X) Nothing  Nothing), (2,2))

                       ,(Triple (Triple Nothing Nothing  Nothing)
                                (Triple Nothing (Just O) (Just X))
                                (Triple Nothing (Just X) Nothing), (2,2))
                       ]

--proof that the AI never loses
testAll :: Bool
testAll = testHere initialBoard where
    testHere board
        | Just O == winner board = True
        | null placements = True
        | otherwise = all (not . computerLost) placements &&
                      (all testHere . map makeMove . filter (not . allFilled) $ placements) where
            placements = map (\x -> place x X board) $ openSpots board
            computerLost = (== Just X) . winner
            makeMove board' = place (suggestMove O board') O board'
