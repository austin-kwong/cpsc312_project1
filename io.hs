import System.Environment

main = do
  s <- readFile "example.txt"
  let tokens = tokenize s in do
    print tokens
    writeFile "sorted.txt" s



-- takes a string and a delimiter, returning just an array of strings

-- default method
tokenize :: String -> [String]
tokenize string = (tokenizeHelper string '\n' [] [])

tokenizeHelper :: String -> Char -> String ->  [String] -> [String]
tokenizeHelper [] c currAcc acc = rev acc
tokenizeHelper (h:t) c currAcc acc
  | h == c = tokenizeHelper t c [] (rev currAcc:acc)
  | otherwise = tokenizeHelper t c (h:currAcc) acc

-- util functions
-- reverse
rev lst = foldl (\acc x -> (x:acc)) [] lst
