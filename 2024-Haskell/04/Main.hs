module Main where

import Data.List (transpose, groupBy, sortOn)

-- parse
parse :: String -> [String]
parse = lines

partA :: String -> Int 
partA = sum . map (sum . map (\ss -> countXmas ss 0)) . (\s -> map ($ parse s) textPerm)

isXmas :: String -> Int
isXmas "XMAS" = 1
isXmas _      = 0

countXmas :: String -> Int -> Int
countXmas [] n       = n
countXmas ('X':xs) n = countXmas xs $ n + (isXmas ('X' : take 3 xs))
countXmas (_:xs) n   = countXmas xs n

textPerm :: [([String] -> [String])]
textPerm = [ leftToRight
           , rightToLeft
           , topToBottom
           , bottomToTop
           , bottomLeftToTopRight
           , topRightToBottomLeft
           , bottomRightToTopLeft
           , topLeftToBottomRight
           ]

leftToRight :: [String] -> [String]
leftToRight = id

rightToLeft :: [String] -> [String]
rightToLeft = map reverse

topToBottom :: [String] -> [String]
topToBottom = transpose

bottomToTop :: [String] -> [String]
bottomToTop = transpose . reverse

bottomLeftToTopRight :: [String] -> [String]
bottomLeftToTopRight = toDiagonal

topRightToBottomLeft :: [String] -> [String]
topRightToBottomLeft = map reverse . toDiagonal

bottomRightToTopLeft :: [String] -> [String]
bottomRightToTopLeft = toDiagonal . reverse

topLeftToBottomRight :: [String] -> [String]
topLeftToBottomRight = map reverse . toDiagonal .  reverse

toDiagonal matrix = map (map snd) grouped
  where
    -- Flatten the matrix with index sums
    indexed = [(i + j, matrix !! i !! j) | i <- [0 .. x - 1], j <- [0 .. y - 1]]
    y = length matrix
    x = length (matrix !! 1)
    -- Group by the diagonal index (sum of i and j)
    grouped = groupBy (\(k1, _) (k2, _) -> k1 == k2) $ sortOn fst indexed   


partB :: String -> Int
partB = findValidA . parse

-- search space exculeds outmost row and columns, 
findValidA :: [String] -> Int
findValidA matrix = sum [ 1 
  | i <- [1 .. (length (head matrix)) - 2]
  , j <- [1 .. (length matrix) - 2]
  , matrix !! j !! i == 'A'
  , isXMAS matrix j i
  ]

-- j is row, i is column
isXMAS :: [String] -> Int -> Int -> Bool
isXMAS matrix j i = (lr == "MS" || lr == "SM") && (rl == "MS" || rl == "SM")
  where 
    lr = [matrix !! (j - 1) !! (i - 1), matrix !! (j + 1) !! (i + 1)] 
    rl = [matrix !! (j + 1) !! (i - 1), matrix !! (j - 1) !! (i + 1)]

main = do
  input <- readFile "input.txt"
  print "Day 02"
  putStr "Part A: "
  print $ partA input
  putStr "Pant B: "
  print $ partB input


test = do
  input <- readFile "example.txt"
  print "Day 02"
  putStr "Part A: "
  print $ partA input
  putStr "Pant B: "
  print $ partB input


