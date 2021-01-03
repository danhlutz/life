module Life where

import           Control.Concurrent (threadDelay)
import           Control.Monad (void)
import           Data.Set (Set)
import qualified Data.Set as S
import           System.Console.ANSI (clearScreen)

type Cell = (Integer, Integer)
type Board = Set Cell

--
type Rule = [Int]

spawnRule :: Rule
spawnRule = [3]

surviveRule :: Rule
surviveRule = [2,3]

-- cell operations
borders :: Cell -> Board
borders (x,y)= S.fromList [
  (x-1,y+1), (x,y+1), (x+1,y+1),
  (x-1,y)  ,          (x+1,y),
  (x-1,y-1), (x,y-1), (x+1,y-1) ]

candidates :: Board -> Board
candidates board = foldr S.union board (borders <$> S.toList board)

neighbors :: Board -> Cell -> Int
-- neighbors b c = length $ S.intersection b (borders c)
neighbors b c = length $ filter memberB (S.toList (borders c))
  where
    memberB cell = S.member cell b

spawn :: Board -> Cell -> Bool
spawn b c = neighbors b c `elem`  spawnRule

survive :: Board -> Cell -> Bool
survive b c =
  S.member c b && neighbors b c `elem` surviveRule

lives :: Board -> Cell -> Bool
lives b c = spawn b c || survive b c

next :: Board -> Board
next b = S.filter (lives b) (candidates b)

-- display

printBoard :: Board -> Int -> Cell -> Cell -> String
printBoard b g (x1,y1) (x2,y2) =
  "GENERATTION: " ++ show g ++ "\n" ++
  "LIVE CELLS: " ++ show (length b) ++ "\n" ++
  mconcat (printRow b x1 x2 <$> [y1..y2])

printRow :: Board -> Integer -> Integer -> Integer -> String
printRow b x1 x2 y = (displayCell b <$> row x1 x2 y) ++ "\n"

displayCell :: Board -> Cell -> Char
displayCell b c = if S.member c b then '*' else ' '

row :: Integer -> Integer -> Integer -> [Cell]
row x1 x2 y
  | x1 == x2 = [(x1,y)]
  | otherwise = (x1,y) : row (x1+1) x2 y

--

generate :: (Board, Int) -> IO (Board, Int)
generate (b,g) = do
  clearScreen
  putStr (printBoard b g (0,0) (70,28))
  threadDelay 200000
  generate (next b, g+1)

run :: Board -> IO ()
run b = void $ generate (b, 0)

-- move designs
flipVertical :: Board -> Board
flipVertical b =
  let flipVals = mkFlipVals (minY b) (maxY b)
      flipper = flipCellVertical flipVals
  in mkBoard $ flipper <$> S.toList b

mkFlipVals :: Integer -> Integer -> [(Integer, Integer)]
mkFlipVals low hi = let vals = [low..hi] in zip vals (reverse vals)

minY :: Board -> Integer
minY b = minimum $ snd <$> S.toList b

maxY :: Board -> Integer
maxY b = maximum $ snd <$> S.toList b

flipCellVertical :: [(Integer, Integer)] -> Cell -> Cell
flipCellVertical vals (x,y) =
  case lookup y vals of
    Nothing -> (x,y)
    Just y' -> (x,y')

-- designs
mkBoard :: [Cell] -> Board
mkBoard = S.fromList

glider :: Board
glider = mkBoard [(2,1), (2,2), (1,0), (1,2), (0, 2)]

gosper :: Board
gosper = mkBoard [
  (1,5),(2,5),(1,6),(2,6),
  (11,5),(11,6),(11,7),
  (12,4),(12,8),
  (13,3),(13,9),
  (14,3),(14,9),
  (15,6),
  (16,4),(16,8),
  (17,5),(17,6),(17,7),
  (18,6),
  (21,3),(21,4),(21,5),
  (22,3),(22,4),(22,5),
  (23,2),(23,6),
  (25,1),(25,2),(25,6),(25,7),
  (35,3),(35,4),
  (36,3),(36,4)
                 ]
copperHead :: Board
copperHead = flipVertical $ mkBoard [
  (2,1),(3,1),(6,1),(7,1),
  (2,2),(3,2),(6,2),(7,2),
  (1,3),(2,3),(3,3),(6,3),(7,3),(8,3),
  (1,4),(2,4),(3,4),(6,4),(7,4),(8,4),
  (1,6),(8,6),
  (1,7),(4,7),(5,7),(8,7),
  (2,8),(3,8),(4,8),(5,8),(6,8),(7,8),
  (4,11),(5,11),
  (4,12),(5,12)
                     ]
