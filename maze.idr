import Data.Stream
import Data.Vect
import Data.List
import Control.Monad.State
import Prelude.IO

--auxiliary functions
randoms : Int -> Stream Int
randoms seed = let seed' = 7507 * seed - 4877 in
                  (seed' `shiftR` 2) :: randoms seed'

--types
record MazeCell where
  constructor MZC
  visited : Bool
  walls : Vect 4 Bool
  coords : (Nat, Nat)

record Maze where
  constructor MZ
  maze : List (List MazeCell)
  size : Nat

--functions
flip : MazeCell -> MazeCell
flip mzc = record {visited $= (not)} mzc

genMaze : Nat -> Maze
genMaze val = MZ (reverse (row val val)) val where
  row : Nat -> Nat -> List (List MazeCell)
  row Z m = [[]]
  row (S k) m = [MZC False (fromList [True, True, True, True]) (x, y) | x <- [(S k)], y <- [m.. 1]] :: row k m

findCell : (Nat, Nat) -> Maze -> Bool
findCell xy xs = elemBy (==) xy (coords . custFold . maze xs) where
  custFold : List (List a) -> List a
  custFold [[]] = []
  custFold xs = (head xs) ++ custFold (tail xs)
