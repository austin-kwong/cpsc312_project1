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

opposite :: ComparisonResult -> ComparisonResult
opposite cr
  | cr == More = Less
  | cr == Less = More
  | otherwise = Equal

-- operations on comparison matrix

-- |cascades the new information
-- 1. Update the exact field
-- 2. Update the inverse field
-- 3. Update implied comparisons
{-
Implied comparisons means the assumption of total ordering, but equality is permitted
  - If Equal, no work to be done
  - If rth item More urgent than cth item,
    - then also More than all (c, c*) where (c, c*) = Equal | More, so set (r, c*) to More as well
    - then c also Less than all (r, c*) where(r, c*) = Less | Equal, so set (c, c*) to Less as Well
  - If rth item Less urgent, the opposite of above is true
-}

-- let m = [[Equal, Unknown, Unknown], [More, Equal, Unknown], [Unknown, Unknown, Equal]]
-- updateComparison 2 1 More m
-- expect: [[Equal,Unknown,Unknown],[More,Equal,Less],[More,More,Equal]]

updateComparison :: Int -> Int -> ComparisonResult -> ComparisonMatrix -> ComparisonMatrix
updateComparison r c n a
  | n == Equal = basicUpdated
  | n == More  = foldr (\(ri, ci) m -> updateField r ci More m) basicUpdated (indicesWhere (\ri ci v -> ri == c && (v == Equal || v == More)) basicUpdated)
    where basicUpdated = updateField r c n (updateField c r (opposite n) a)



-- update the field in a given matrix, where
-- - r is the row
-- - c is the column
-- - n is the new value
-- - a is the matrix
updateField :: Int -> Int -> ComparisonResult -> ComparisonMatrix -> ComparisonMatrix
updateField r c n a = [[if (columnIndex == c && rowIndex == r) then n else value  | (columnIndex, value) <- zipWithIndex row]  |(rowIndex, row) <- zipWithIndex a]

-- |returns the list of indices where a given predicate on the tuple (row, column, Value) is true
indicesWhere :: (Int -> Int -> ComparisonResult -> Bool) -> ComparisonMatrix -> [(Int, Int)]
indicesWhere p m = [(ri, ci) | (ri, row) <- zipWithIndex m, (ci, value) <- (zipWithIndex row), p ri ci value]

-- |returns the transformed matrix applied onto the fields which satisfy the predicate
mapWhere :: (Int -> Int -> ComparisonResult -> Bool) -> (Int -> Int -> ComparisonResult -> ComparisonResult) -> ComparisonMatrix -> ComparisonMatrix
mapWhere p f m = [[if (p ri ci value) then  f ri ci value else value | (ci ,value) <- zipWithIndex row]|  (ri, row) <- zipWithIndex m]

-- |returns the item at the given row and column (unsafe!)
getAt r c m = m!!r!!c

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
