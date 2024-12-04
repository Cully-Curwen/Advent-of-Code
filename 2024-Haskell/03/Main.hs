module Main where

-- parse
parse :: String -> [[Int]]
parse = map (map read . words) . lines

partA :: String -> String
partA = const "---"
    
partB :: String -> String
partB = const "---"

main = do
  input <- readFile "input.txt"
  print "Day 02"
  putStr "Part A: "
  print $ partA input
  putStr "Pant B: "
  print $ partB input
