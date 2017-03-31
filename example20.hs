import System.IO
import Control.Monad.Cont
import Data.Char

toIO :: a -> IO a
toIO x = return x

fun :: IO String
fun = do n <- (readLn:: IO Int)
         convert n

convert :: Int -> IO String
convert n = (`runCont` id) $ do
              str <- callCC $ \exit1 -> do
                when (n < 10) (exit1 $ toIO (show n))
                let ns = map digitToInt (show (n `div` 2))
                n' <- callCC $ \exit2 -> do
                  when ((length ns) < 3) (exit2 (toIO (length ns)))
                  when ((length ns) < 5) (exit2 $ do putStrLn "Enter a number:"
                                                     x <- (readLn::IO Int)
                                                     return x)
                  when ((length ns) < 7) $ do let ns' = map intToDigit (reverse ns)
                                              exit1 $ toIO (dropWhile (=='0') ns')
                  return (toIO (sum ns))
                return $ do num <- n' -- this is an IO monad block
                            return $ "(ns = " ++ (show ns) ++ ") " ++ (show  num)
              return $ do s <- str -- this is an IO monad block
                          return $ "Answer: " ++ s
