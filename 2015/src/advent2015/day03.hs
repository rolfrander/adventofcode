-- -*- mode: haskell -*-

-- The seemingly straight forward way of solving this puzzle, is to
-- construct a large 2D-array, start out in the middle and move
-- around, marking in the array where we walk and count the number of
-- marks. However, a slightly more elegant solution is to save the
-- coordinates we pass as we move along and count the number of unique
-- points passed.

import qualified Data.Set as Set
import qualified System.IO as IO

-- points and movement-operations
type Point = (Int, Int)

up :: Point -> Point
up (x, y) = (x, (y+1))

down :: Point -> Point
down (x, y) = (x, (y-1))

left :: Point -> Point
left (x, y) = ((x-1), y)

right :: Point -> Point
right (x, y) = ((x+1), y)

-- transforms a character to a move-function
move :: Char -> Point -> Point
move '<' = left
move '>' = right
move '^' = up
move 'v' = down

-- the walker-function maps a list of input-characters <>^v to a list
-- of move-functions
walker :: [Char] -> [Point -> Point]
walker = map move

-- domove starts out at a point, moves according to a list of
-- move-functions and returns a list of points passed, including the
-- start and end positions
domove :: Point -> [Point->Point] -> [Point]
domove start x = scanl (flip ($)) start x

-- counts the number of unique elements in a list by converting to a
-- Set
countUniq :: (Ord a) => [a] -> Int
countUniq = Set.size . Set.fromList

moveAndCollect :: [Char] -> Set.Set Point
moveAndCollect input = Set.fromList (domove (0,0) (walker input))

-- performs moves as specified by a list of move-chars (<>^v) and
-- counts the number of unique positions visited
moveAndCount :: [Char] -> Int
moveAndCount input = Set.size (moveAndCollect input)

justOdd :: [a] -> [a]
justOdd x = map snd (filter fst (zip (cycle [False, True]) x))

justEven :: [a] -> [a]
justEven x = map snd (filter fst (zip (cycle [True, False]) x))

moveAndCountDouble :: [Char] -> Int
moveAndCountDouble input = let santa = moveAndCollect (justEven input)
                               robo  = moveAndCollect (justOdd input)
                           in Set.size (Set.union santa robo)

--moveAndCountFile :: FilePath -> IO Int
moveAndCountFile filename = do c <- IO.readFile filename
                               return (moveAndCount c)

--moveAndCountDoubleFile :: FilePath -> IO Int
moveAndCountDoubleFile filename = do c <- IO.readFile filename
                                     return (moveAndCountDouble c)
