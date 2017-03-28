import Monad
import Directory
import System

-- NOTE: doesDirectoryExist has type FilePath -> IO Bool

-- this program prints only the directories named on the command line
main :: IO ()
main = do names <- getArgs
          dirs  <- filterM doesDirecotryExist names
          mapM_ putStrLn dirs
