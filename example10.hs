import Control.Monad
import Data.Maybe

data Record = Rec {name::String, age::Int} deriving Show
type DB = [Record]

-- getYoungerThan returns all records for people younger than a specified age.
-- It uses the guard function to eliminate records for ages at or over the limit.
-- This is just for demonstration purposes. In real life, it would be
-- clearer to simply use filter, When the filter criteria are more complex,
-- guard becomes more useful.
getYoungerThan :: Int -> DB -> [Record]
getYoungerThan limit db = mapMaybe (\r -> do { guard (age r < limit); return r}) db
