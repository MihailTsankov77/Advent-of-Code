-- O1-mini
-- how to import a function into another file

module FileUtils (readFileAndSplit) where

-- O1-mini
-- Write me Haskell function that reads from file and splits returns array of arrays:
-- How to create the array:
-- first split by row
-- then split by empty space
-- the answer should be words x lines

readFileAndSplit :: FilePath -> IO [[String]]
readFileAndSplit path = do
  contents <- readFile path
  let linesOfFile = lines contents
  let wordsInLines = map words linesOfFile
  return wordsInLines