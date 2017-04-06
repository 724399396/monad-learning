class Fluffy f where
  furry :: (a -> b) -> f a -> f b

-- Exercises 1
-- Relative Difficulty: 1
instance Fluffy [] where
  furry _ [] = []
  furry f (x:xs) = f x : furry f xs

-- Exercises 2
-- Relative Difficulty: 1
instance Fluffy Maybe where
  furry _ Nothing = Nothing
  furry f (Just x) = Just (f x)

-- Exercises 3
-- Relative Difficulty: 5
instance Fluffy ((->) t) where
  furry f g = \x -> f (g x)

newtype EitherLeft b a = EitherLeft (Either a b)
newtype EitherRight a b = EitherRight (Either a b)

-- Exercise 4
-- Relative Difficulty: 5
instance Fluffy (EitherLeft t) where
  furry f (EitherLeft (Left x)) = EitherLeft (Left (f x))
  furry _ (EitherLeft (Right x)) = EitherLeft (Right x)


-- Exercise 5
-- Relative Difficulty: 5
instance Fluffy (EitherRight t) where
  furry f (EitherRight (Right x)) = EitherRight (Right (f x))
  furry _ (EitherRight (Left x)) = EitherRight (Left x)

class Misty m where
  banana :: (a -> m b) -> m a -> m b
  unicorn :: a -> m a
  -- Exercise 6
  -- Relative Difficulty: 3
  -- (use banana and/or unicorn)
  furry' :: (a -> b) -> m a -> m b
  furry' f ma = banana (\a -> unicorn (f a)) ma

-- Exercise 7
-- Relative Difficulty: 2
instance Misty [] where
  banana _ [] = []
  banana f (x:xs) = f x ++ (banana f xs)
  unicorn x = [x]

-- Exercise 8
-- Relative Difficulty: 2
instance Misty Maybe where
  banana _ Nothing = Nothing
  banana f (Just x) = f x
  unicorn = Just

-- Exercise 9
-- Relative Difficulty: 6
instance Misty ((->) t) where
  banana f g =  (\x -> f (g x) x)
  unicorn a = (\_ -> a)

-- Exercise 10
-- Relative Difficulty: 6
instance Misty (EitherLeft t) where
  banana f (EitherLeft (Left x)) = f x
  banana _ (EitherLeft (Right x)) = EitherLeft (Right x)
  unicorn x = EitherLeft (Left x)

-- Exercise 11
-- Relative Difficulty: 6
instance Misty (EitherRight t) where
  banana f (EitherRight (Right x)) = f x
  banana _ (EitherRight (Left x)) = EitherRight (Left x)
  unicorn x = EitherRight (Right x)

-- Exercise 12
-- Relative Difficulty: 3
jellybean :: (Misty m) => m (m a) -> m a
jellybean = banana id

-- Exercise 13
-- Relative Difficulty: 6
apple :: (Misty m) => m a -> m (a -> b) -> m b
apple ma mf = flip banana ma $ (\a ->
                   flip banana mf $ (\f ->
                        unicorn $ f a))

-- Exercise 14
-- Relative Difficulty: 6
moppy :: (Misty m) => [a] -> (a -> m b) -> m [b]
moppy [] _ = unicorn []
moppy (x:xs) f = flip banana (f x) (\y ->
                      flip banana (moppy xs f) (\ys ->
                           unicorn (y:ys)))

-- Exercise 15
-- Relative Difficulty: 6
-- (bonus: use moppy)
sausage :: (Misty m) => [m a] -> m [a]
sausage ma =  moppy ma id

-- Exercise 16
-- Relative Difficulty: 6
-- (bonus: use apple + furry')
banana2 :: (Misty m) => (a -> b -> c) -> m a -> m b -> m c
banana2 f ma mb = apple mb (furry' f ma)

-- Exercise 17
-- Relative Difficulty: 6
-- (bonus: use apple + banana2)
banana3 :: (Misty m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
banana3 f ma mb mc = apple mc (banana2 f ma mb)

-- Exercise 18
-- Relative Difficulty: 6
-- (bonus: use apple + banana3)
banana4 :: (Misty m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
banana4 f ma mb mc md = apple md (banana3 f ma mb mc)

newtype State s a = State {
  state :: (s -> (s, a))
}

-- Exercise 19
-- Relative Difficulty: 9
instance Fluffy (State s) where
  furry f old = State $ \st ->
                          let (st',a') = (state old) st
                          in
                            (st', f a')

-- Exercise 20
-- Relative Difficulty: 10
instance Misty (State s) where
  banana f old = State $ \st ->
                          let (st',a') = (state old) st
                          in
                            (state (f a')) st'
  unicorn a = State $ \s -> (s, a)
