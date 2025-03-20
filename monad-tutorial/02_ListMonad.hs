{-
  The List Monad in Haskell
  
  The List monad represents computations with multiple possible results.
  It's a great example of non-deterministic computation.
-}

module ListMonad where

-- In Haskell, lists are already defined as a monad where:
-- return x = [x]
-- xs >>= f = concat (map f xs)

-- Example: Finding all possible pairs from two lists
pairs :: [a] -> [b] -> [(a, b)]
pairs xs ys = do
  x <- xs  -- For each x in xs
  y <- ys  -- For each y in ys
  return (x, y)  -- Return the pair (x, y)

-- This is equivalent to:
pairsDesugared :: [a] -> [b] -> [(a, b)]
pairsDesugared xs ys =
  xs >>= \x ->
  ys >>= \y ->
  return (x, y)

-- Or using list comprehension (which is syntactic sugar for the list monad):
pairsComprehension :: [a] -> [b] -> [(a, b)]
pairsComprehension xs ys = [(x, y) | x <- xs, y <- ys]

-- Example: Finding Pythagorean triples
pythagoreanTriples :: Int -> [(Int, Int, Int)]
pythagoreanTriples n = do
  a <- [1..n]
  b <- [a..n]  -- Ensure b >= a to avoid duplicates
  c <- [b..n]  -- Ensure c >= b
  if a*a + b*b == c*c  -- Check if it's a Pythagorean triple
    then return (a, b, c)
    else []  -- Empty list (no results) if not a triple

-- The power of the list monad is that it handles the nested loops and filtering automatically

-- Example: Finding all paths in a graph
type Graph = [(Int, Int)]  -- Edges represented as pairs of vertices

paths :: Graph -> Int -> Int -> [[Int]]
paths graph start end = paths' [start]
  where
    paths' [] = []  -- Handle empty list case
    paths' visited@(current:_)
      | current == end = return visited
      | otherwise = do
          -- Find all neighbors of the current vertex that haven't been visited
          (from, to) <- graph
          if from == current && to `notElem` visited
            then paths' (to : visited)
            else []

-- Example graph: 1 -> 2 -> 3, 1 -> 3
exampleGraph :: Graph
exampleGraph = [(1, 2), (2, 3), (1, 3)]

main :: IO ()
main = do
  putStrLn "List Monad Examples:"
  
  putStrLn "\nAll pairs from [1,2] and ['a','b']:"
  print $ pairs [1, 2] ['a', 'b']
  
  putStrLn "\nPythagorean triples up to 20:"
  print $ pythagoreanTriples 20
  
  putStrLn "\nPaths from 1 to 3 in the example graph:"
  -- Reverse each path to show them in the correct order
  print $ map reverse $ paths exampleGraph 1 3
