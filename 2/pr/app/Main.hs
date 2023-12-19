module Main where

import Control.Parallel.Strategies (parMap, rpar)
import Data.List (foldl', maximumBy, sortBy)
import Data.Ord (comparing)

type Matrix = [[Double]]
type Vector = [Double]

-- Gauss
partialPivotGaussianElimination :: Matrix -> Vector -> Vector
partialPivotGaussianElimination matrix vector =
  backwardSubstitution upperTriangular augmentedVector
  where
    augmentedMatrix = zipWith (++) matrix (map return vector)
    upperTriangular = forwardElimination augmentedMatrix
    augmentedVector = map last augmentedMatrix

-- Forward elimination 
forwardElimination :: Matrix -> Matrix
forwardElimination matrix = foldl' eliminateRow matrix $ zip [0..] matrix
  where
    eliminateRow :: Matrix -> (Int, [Double]) -> Matrix
    eliminateRow mat (i, row)
      | pivot == 0 = error "Division by zero during forward elimination"
      | otherwise  = parMap rpar (\r -> zipWith (\x y -> y - multiplier * x) row r) mat
      where
        pivot = head $ drop i row
        multiplier = 1 / pivot

-- Backward substitution 
backwardSubstitution :: Matrix -> Vector -> Vector
backwardSubstitution matrix vector = reverse $ foldl' backSubs [last vector] (zip [size-2, size-3 .. 0] [size-1, size-2 .. 0])
  where
    size = length matrix
    backSubs :: Vector -> (Int, Int) -> Vector
    backSubs acc (i, j)
      | last (matrix !! i) == 0 = error "Division by zero during backward substitution"
      | otherwise = x : acc
      where
        x = (vector !! i - sum (zipWith (*) (init (matrix !! i)) acc)) / last (matrix !! i)

-- Perform Gaussian elimination with complete pivoting
completePivotGaussianElimination :: Matrix -> Vector -> Vector
completePivotGaussianElimination matrix vector =
  backwardSubstitution upperTriangular augmentedVector
  where
    augmentedMatrix = zipWith (++) matrix (map return vector)
    upperTriangular = foldl' eliminateRow augmentedMatrix [0..size-1]
    augmentedVector = map last upperTriangular
    size = length matrix

    eliminateRow :: Matrix -> Int -> Matrix
    eliminateRow mat k =
      if pivot == 0
        then error $ "Division by zero during complete pivoting in row " ++ show k
        else parMap rpar (\r -> zipWith (\x y -> y - multiplier * x) pivotRow r) mat'
      where
        (maxIndex, _) = maxAbsIndex (mat !! k)
        pivotRow = mat !! k
        mat' = sortBy (comparing (\row -> row !! maxIndex)) mat
        pivot = head pivotRow
        multiplier = 1 / pivot

-- Find the index of the maximum absolute value in a list along with its value
maxAbsIndex :: [Double] -> (Int, Double)
maxAbsIndex xs = maximumBy (comparing snd) (zip [0..] (map abs xs))

-- Main function
main :: IO ()
main = do
  let matrix = [[2, 1, 3], [3, 2, 5], [1, 4, 6]]
      vector = [9, 16, 24]

  putStrLn "Partial Pivoting Gaussian Elimination:"
  let partialPivotSolution = partialPivotGaussianElimination matrix vector
  putStrLn $ "Solution: " ++ show partialPivotSolution

  putStrLn "\nComplete Pivoting Gaussian Elimination:"
  let completePivotSolution = completePivotGaussianElimination matrix vector
  putStrLn $ "Solution: " ++ show completePivotSolution
