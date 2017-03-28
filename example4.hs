-- a Dict is just a finite map from strings to strings
type Dict = FiniteMap String String

-- this an auxilliary function used with foldl
addEntry :: Dict -> Entry -> Dict
addEntry d e = addToFM d (key e) (value e)

-- this is an auxiliiary function used with foldM inside the IO monad
addDataFromFile :: Dict -> Handle -> IO Dict
addDataFromFile dict hdl = do contents <- hGetContents hdl
                              entries <- return (map read (lines contents))
                              return (foldl (addEntry) dict entries)

-- this program builds a dictionary from the entries in all files named on the
-- command line and then prints it out as an association list
main :: IO ()
main = do files   <- getArgs
          handles <- mapM openForReading files
          dict    <- foldM addDataFromFile emptyFM handles
          print (fmToList dict)
