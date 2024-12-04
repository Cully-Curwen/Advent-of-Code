module Main where

-- parse
parse :: String -> [[Int]]
parse = map (map read . words) . lines

-- The levels are either all increasing or all decreasing.
-- Any two adjacent levels differ by at least one and at most three.
partA :: String -> Int
partA = sum . map (safe . diff) . parse
    
safe :: [Int] -> Int
safe xs = if safeMag xs && (constPos xs || constNeg xs) then 1 else 0
  where
    constPos xs = and[x > 0 | x <- xs]
    constNeg xs = and[x < 0 | x <- xs]
    safeMag xs = and[abs x <= 3 | x <- xs]

diff :: [Int] -> [Int]
diff xs = [x - y|(x,y) <- pairs xs]

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

-- partB
partB :: String -> Int
partB = sum . map (safe') . parse
  where
    safe' xs = if (1 ==) (safe (diff xs))
      then 1
      else if any (1 ==) [safe (diff x) | x <- removeOne xs]
        then 1
        else 0

removeOne :: [a] -> [[a]]
removeOne xs = [take i xs ++ drop (i + 1) xs | i <- [0..length xs - 1]]

main = do
  input <- readFile "input.txt"
  print "Day 02"
  putStr "Part A: "
  print $ partA input
  putStr "Pant B: "
  print $ partB input
