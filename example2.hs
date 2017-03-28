-- we can also use do-notation to build complicated sequences
mothersPaternalGrandfather :: Sheep -> Maybe Sheep
mothersPaternalGrandfather s = do m <- mother s
                                  gf <- father m
                                  father gf

mothersPaternalGrandfather s = mother s >>= father >>= father

parent :: Sheep -> Maybe Sheep
parent s = father s `mplus` mother s

grandparent s = parent s >>= parent

parent :: Sheep -> [Sheep]
parent s = maybeToList (father s) `mplus` maybeToList (mother s)

grandparent s = parent s >>= parent


parent :: (MonadPlus m) => Sheep -> m Sheep
parent s = toMonad (father s) `mplus` toMonad (mother s)

toMonad Nothing = mzero
toMonad (Just x) = return x

grandparent s = parent s >>= parent
