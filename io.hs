import System.Environment

main = do
  [f]<- getArgs
  s <- readFile f
  writeFile "sorted.txt" s
  
