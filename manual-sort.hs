import System.Environment

-- Insertion sort methods
--------------------------------------------
--------------------------------------------
main_insertion = do
  s <- readFile "example.txt"
  let tokens = (shuffleList (tokenize s))
  let comparisonMatrix = [[if (c /= r) then Unknown else Equal | (c, _) <- zipWithIndex tokens] | (r, _) <- zipWithIndex tokens]
  sortedTasks <- insertionSort (zipWithIndex tokens) [] comparisonMatrix
  let output = collapseStrings sortedTasks
  writeFile "output.txt" output
  putStrLn "Here is the new order: "
  putStrLn output

-- Insertion sort algorithm
insertionSort :: [(Int, String)] -> [(Int, String)] -> ComparisonMatrix -> IO [String]
insertionSort [] sorted _ = return (map (\(_, y) -> y) sorted)
insertionSort (h:t) sorted comparisonMatrix =
  do
    (newMatrix, newSorted) <- insertInto h comparisonMatrix sorted
    insertionSort t newSorted newMatrix


-- Recursively insert items into a sorted list, caching comparisons for later use
insertInto :: (Int, String) -> ComparisonMatrix -> [(Int, String)] -> IO (ComparisonMatrix, [(Int, String)])
insertInto (index, value) matrix [] = return (matrix, [(index, value)])
insertInto (row, insertValue) matrix ((column, sortedValue): t) = 
      case (getAt row column matrix) of
        Unknown -> do
            putStrLn ("Is {" ++ insertValue ++ "} [M]ore or [L] than {" ++ sortedValue ++ "}? or [I]ndifferent")
            input <- getLine
            case () of
              () | input `elem` ["M", "m"] -> insertInto (row, insertValue)  (updateComparison row column More matrix) ((column, sortedValue): t)
                 | input `elem` ["L", "l"] -> insertInto (row, insertValue) (updateComparison row column Less matrix) ((column, sortedValue): t)
                 |  otherwise -> insertInto (row, insertValue) (updateComparison row column Equal matrix) ((column, sortedValue): t)
              
        Less -> do
          (newMatrix, sortedRest) <- (insertInto (row, insertValue) matrix t)--put deeper in list
          return (newMatrix, (column, sortedValue):sortedRest)
          
        otherwise -> return (matrix, ((row, insertValue):(column, sortedValue): t)) -- place at beginning of list and return


-- Merge sort methods
--------------------------------------------
--------------------------------------------

main_merge = do
  s <- readFile "example.txt"
  let tokens = (shuffleList (tokenize s))
  let comparisonMatrix = [[if (c /= r) then Unknown else Equal | (c, _) <- zipWithIndex tokens] | (r, _) <- zipWithIndex tokens]
  sortedTasks <- mergeSortConverter tokens comparisonMatrix
  let output = collapseStrings sortedTasks
  writeFile "output.txt" output
  putStrLn "Here is the new order: "
  putStrLn output

-- Merge sort algorithm
-- Strips the indices off the elements in the list return by mergeSortHelper
mergeSortConverter :: [String] -> ComparisonMatrix -> IO [String]
mergeSortConverter tokens comparisonMatrix =
  do
    sorted <- mergeSort (zipWithIndex tokens) comparisonMatrix
    return (map (\(_, y) -> y) sorted)

-- This is the actual merge sort algorithm
mergeSort :: [(Int, String)] -> ComparisonMatrix -> IO [(Int, String)]
mergeSort [] _ = return []
mergeSort [x] _ = return [x]
mergeSort xs matrix =
  do
    fstHalfSorted <- (mergeSort (fstHalf xs) matrix)
    sndHalfSorted <- (mergeSort (sndHalf xs) matrix)
    (newMatrix, sorted) <- merge fstHalfSorted sndHalfSorted matrix
    return sorted

-- Recursively merge items from two sorted lists, caching comparisons for later use
merge :: [(Int, String)] ->  [(Int, String)] -> ComparisonMatrix -> IO (ComparisonMatrix, [(Int, String)])
merge xs [] matrix = return (matrix, xs)
merge [] ys matrix = return (matrix, ys)
merge ((row, value1):xs) ((column, value2):ys) matrix =
    case (getAt row column matrix) of
      Unknown -> do
          putStrLn("Is {" ++ value1 ++ "} [M]ore or [L] than {" ++ value2 ++ "}? or [I]ndifferent")
          input <- getLine
          case () of
            () | input `elem` ["M", "m"] -> do 
                  (newMatrix, sorted) <- (merge xs ((column, value2):ys) (updateComparison row column More matrix))
                  return (newMatrix, (row, value1):sorted)
               | input `elem` ["L", "l"] -> do
                  (newMatrix, sorted) <- (merge ((row, value1):xs) ys (updateComparison row column Less matrix))
                  return (newMatrix, (column, value2):sorted)
               | otherwise -> do
                  (newMatrix, sorted) <- (merge xs ((column, value2):ys) (updateComparison row column Equal matrix))
                  return (newMatrix, (row, value1):sorted)

      Less -> do
          (newMatrix, sorted) <- (merge ((row, value1):xs) ys matrix)
          return (newMatrix,(column, value2):sorted)

      otherwise -> do
          (newMatrix, sorted) <- (merge xs ((column, value2):ys) matrix)
          return (newMatrix,  (row, value1):sorted)


-- Other helper methods and data definitions
--------------------------------------------
--------------------------------------------

-- data definitions for comparison matrix
data ComparisonResult = Less | Equal | More | Unknown deriving (Read, Enum, Eq)
type ComparisonMatrix = [[ComparisonResult]]

instance Show ComparisonResult where
    show Equal = "   Equal   "
    show Less = "   Less    "
    show More = "   More    "
    show Unknown = "  Unknown  "

-- prints matrix nicely
printmatrix m = putStrLn (unlines (map show m))

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


updateComparison :: Int -> Int -> ComparisonResult -> ComparisonMatrix -> ComparisonMatrix
updateComparison r c comp mat
  | comp == Equal = basicUpdated
  | otherwise = foldr 
                  (\(ri, ci) m -> if (getAt r ci m /= comp) then (updateComparison r ci comp m) else m) 
                  basicUpdated 
                  (indicesWhere (\ri _ v -> ri == c && (v == Equal || v == comp)) basicUpdated)
      where basicUpdated = updateField r c comp (updateField c r (opposite comp) mat)

-- Try:
--
-- let m = [[Equal, Unknown, Unknown], [Equal, Equal, Unknown], [Unknown, Unknown, Equal]]
-- updateComparison 2 1 More m
-- expect: [[Equal,Unknown,Unknown],[More,Equal,Less],[More,More,Equal]]
--
-- let m = [[Equal, Equal, Unknown, Unknown], [Equal, Equal, Less, Unknown], [Unknown, More, Equal, Unknown], [Unknown, Unknown, Unknown, Equal]]
-- [   Equal   ,   Equal   ,  Unknown  ,  Unknown  ]
-- [   Equal   ,   Equal   ,   Less    ,  Unknown  ]
-- [  Unknown  ,   More    ,   Equal   ,  Unknown  ]
-- [  Unknown  ,  Unknown  ,  Unknown  ,   Equal   ]
-- updateComparison 3 2 More m
-- expect: 
-- [   Equal   ,   Equal   ,  Unknown  ,   Less    ]
-- [   Equal   ,   Equal   ,   Less    ,   Less    ]
-- [  Unknown  ,   More    ,   Equal   ,   Less    ]
-- [   More    ,   More    ,   More    ,   Equal   ]

-- let m = [[Equal, More, Unknown, Unknown], [Equal, Equal, More, Unknown], [Unknown, Less, Equal, Unknown], [Unknown, Unknown, Unknown, Equal]]




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
  | h == c = if (currAcc /= [])
    then tokenizeHelper t c [] (rev currAcc:acc)
    else tokenizeHelper t c [] acc
  | otherwise = tokenizeHelper t c (h:currAcc) acc

-- util functions
-- reverse
rev lst = foldl (\acc x -> (x:acc)) [] lst

-- zip-with-index
zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex lst = zip [0..(length lst)] lst

-- collapse a list of strings to a single newline seperated string
collapseStrings lst = foldr (\x acc -> x ++ "\n" ++ acc) "" lst

-- gives the first half of a list
fstHalf lst = fst (splitList lst)
-- gives the second half of a list
sndHalf lst = snd (splitList lst)

-- split list
-- |returns a tuple of (floor n/2) elements and (ceil n/2) elements
splitList :: [a] -> ([a], [a])
splitList [] = ([], [])
splitList lst = splitListHelper lst (floor (fromIntegral (length lst) / 2)) []

splitListHelper :: [a] -> Int -> [a] -> ([a], [a])
splitListHelper lst 0 acc = (acc, lst)
splitListHelper (h:t) n acc = splitListHelper t (n - 1) (acc++[h]) 


-- List Randomizer
-- constant declarations

multiplier = 18342
increment  = 13824
modulus    = 71342
seed       = 19394

-- function to produce the next pseudo-random number
nextRand :: Int -> Int
nextRand n = (multiplier * n + increment) `mod` modulus

-- creates a generator for an infinite of random numbers
randomSequence = iterate nextRand

-- shuffles the list using the random sequence
shuffleList lst = shuffleListHelper lst (map (\(x,y) -> (y `mod` x)) (zip (rev [1..(length lst)]) (take (length lst) (randomSequence seed))))

shuffleListHelper [] indicesToTake = []
shuffleListHelper lst [] = lst
shuffleListHelper lst (h:t) =
  let (value, restOfList) = removeAndTakeAt lst h
  in (value: (shuffleListHelper restOfList t))

-- helper function for modifiying a list
removeAndTakeAt lst n = (lst!!n, [x | (i, x) <- (zipWithIndex lst), i /= n ])
