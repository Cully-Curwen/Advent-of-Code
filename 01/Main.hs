module Main where

import Data.List (sort)

-- 1. read input file
-- 2. parse two outpust per line into seperate lists
parse :: String -> [(Int, Int)]
parse = map (fn . words) . lines
  where
    fn [x, y] = (read x, read y)
    fn _ = (0, 0)

-- 3. sort list in asseding order
-- 4. zip list
sortZip :: [(Int, Int)] -> [(Int, Int)]
sortZip = (\(xs, ys) -> zip (sort xs) (sort ys)) . unzip 

-- 5. get differece between tuple numbers
-- 6. sum vaues
diffSum :: [(Int, Int)] -> Int
diffSum = sum . map (\(x, y) -> abs (x - y))


partA :: String -> Int
partA = diffSum . sortZip . parse

similarityScore :: ([Int], [Int]) -> Int
similarityScore (xs, ys) = sum[y|x <- xs, y <- ys, x == y]  

partB :: String -> Int
partB = similarityScore . unzip . parse 

main = do
  input <- readFile "input.txt"
  print "Day 01"
  putStr "Part A: "
  print $ partA input
  putStr "Pant B: "
  print $ partB input
