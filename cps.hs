import Control.Monad.Trans.Cont
import Control.Monad
import qualified Control.Monad.Cont as C
import Data.Char

add :: Int -> Int -> Int
add x y = x + y

square :: Int -> Int
square x = x * x

pythagoras :: Int -> Int -> Int
pythagoras x y = add (square x) (square y)

add_cps :: Int -> Int -> (Int -> r) -> r
add_cps x y = \k -> k (add x y)

square_cps :: Int -> (Int -> r) -> r
square_cps x = \k -> k (square x)

pythagoras_cps :: Int -> Int -> (Int -> r) -> r
pythagoras_cps x y = \k ->
  square_cps x $ \square_x ->
  square_cps y $ \square_y ->
  add_cps square_x square_y $ k

thrice :: (a -> a) -> a -> a
thrice f x = f (f (f x))

thrice_cps :: (a -> ((a -> r) -> r)) -> a -> ((a -> r) -> r)
thrice_cps f_cps x = \k ->
  f_cps x $ \fx ->
  f_cps fx $ \ffx ->
  f_cps ffx $ k

chainCPS :: ((a -> r) -> r) -> (a -> ((b->r) ->r)) -> ((b -> r) -> r)
chainCPS s f = \k -> s $ \x -> f x $ k

{- cont :: ((a -> r) -> r) -> Cont r a
runCont :: Count r a -> (a -> r) -> r

instance Monad (Cont r) where
  return x = cont ($ x)
  s >>= f = cont $ \c -> runCont s $ \x -> runCont (f x) c

callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
callCC f = cont $ \h -> runCont (f (\a -> cont $ \_ -> h a)) h
-}

add_cont :: Int -> Int -> Cont r Int
add_cont x y = return (add x y)

square_cont :: Int -> Cont r Int
square_cont x = return (square x)

pythagoras_cont :: Int -> Int -> Cont r Int
pythagoras_cont x y = do
  x_squared <- square_cont x
  y_squared <- square_cont y
  add_cont x_squared y_squared

-- Without callCc
square_cont_normal :: Int -> Cont r Int
square_cont_normal n = return (n ^ 2)

-- With callCC
squareCCC :: Int -> Cont r Int
squareCCC n = callCC $ \k -> k (n ^ 2)

foo :: Int -> Cont r String
foo x = callCC $ \k -> do
  let y = x ^ 2 + 3
  when (y > 20) $ k "over twenty"
  return (show $ y - 4)

bar :: Char -> String -> Cont r Int
bar c s = do
  msg <- callCC $ \k -> do
    let s0 = c : s
    when (s0 == "hello") $ k "They say hello."
    let s1 = show s0
    return ("They appear to be saying " ++ s1)
  return (length msg)

quux :: Cont r Int
quux = callCC $ \k -> do
  let n = 5
  k n
  return 25

{- We use the continuation monad to perform "escapes" from code blocks.
This function implements a complicated control structrue to process
numbers:

Input (n)        Output                         List Shown
=============    ======                        ============
n-9              n                              none
10-199           number of digits in (n/2)      digits of (n/2)
200-19999        n                              digits of (n/2)
20000-1999999    (n/2) backwards                none
>= 2000000       sum of digits of (n/2)         digits of (n/2)
-}
fun :: Int -> String
fun n = (`runCont` id) $ do
  str <- callCC $ \exit1 -> do
    when (n < 10) (exit1 (show n))
    let ns = map digitToInt (show (n `div` 2))
    n' <- callCC $ \exit2 -> do
      when ((length ns) < 3) (exit2 (length ns))
      when ((length ns) < 5) (exit2 n)
      when ((length ns) < 7) $ do
        let ns' = map intToDigit (reverse ns)
        exit1 (dropWhile (=='0') ns')
      return $ sum ns
    return $ "(ns = " ++ (show ns) ++ ") " ++ (show n')
  return $ "Answer: " ++ str

divExcpt :: Int -> Int -> (String -> Cont r Int) -> Cont r Int
divExcpt x y handler = callCC $ \ok -> do
  err <- callCC $ \notOk -> do
    when (y == 0) $ notOk "Denominator 0"
    ok $ x `div` y
  handler err

tryCont :: C.MonadCont m => ((err -> m a) -> m a) -> (err -> m a) -> m a
tryCont c h = C.callCC $ \ok -> do
  err <- C.callCC $ \notOk -> do
      x <- c notOk
      ok x
  h err

data SqrtException = LessThanZero deriving (Show, Eq)

sqrtIO :: (SqrtException -> ContT r IO ()) -> ContT r IO ()
sqrtIO throw = do
  ln <- C.lift (putStr "Enter a number to sqrt: " >> readLn)
  when (ln < 0) (throw LessThanZero)
  C.lift $ print (sqrt ln)

main = runContT (tryCont sqrtIO (C.lift . print)) return  
