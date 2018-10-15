import System.Environment

main = do
  s <- readFile "example.txt"
  let tokens = tokenize s
  let comparisonMatrix = [[if (c /= r) then Unknown else Equal | (c, _) <- zipWithIndex tokens] | (r, _) <- zipWithIndex tokens]
  print comparisonMatrix
  writeFile "sorted.txt" s

-- data definitions for comparison matrix
data ComparisonResult = Less | Equal | More | Unknown deriving (Read, Show, Enum, Eq, Ord)
type ComparisonMatrix = [[ComparisonResult]]

-- operations on comparison matrix
-- update the field in a given matrix, where
-- - r is the row
-- - c is the column
-- - n is the new value
-- - a is the matrix
updateField :: Int -> Int -> ComparisonResult -> ComparisonMatrix -> ComparisonMatrix
updateField r c n a = [[if (columnIndex == c && rowIndex == r) then n else value  | (columnIndex, value) <- zipWithIndex row]  |(rowIndex, row) <- zipWithIndex a]

-- Tokenizing
-- takes a string and a delimiter, returning just an array of strings
-- default method
tokenize :: String -> [String]
tokenize string = (tokenizeHelper string '\n' [] [])
-- helper
tokenizeHelper :: String -> Char -> String ->  [String] -> [String]
tokenizeHelper [] c currAcc acc = rev acc
tokenizeHelper (h:t) c currAcc acc
  | h == c = tokenizeHelper t c [] (rev currAcc:acc)
  | otherwise = tokenizeHelper t c (h:currAcc) acc

-- util functions
-- reverse
rev lst = foldl (\acc x -> (x:acc)) [] lst

-- zip-with-index
zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex lst = zip [0..(length lst)] lst
