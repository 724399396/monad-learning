type Variable = String
type Value = String
type EnvironmentStack = [[(Variable,Value)]]

-- lookupVar retrieves a variable's value from the environment stack
-- It uses msum in the Maybe monad to return the first non-Nothing value.
lookupVar :: Variable -> EnvironmentStack -> Maybe Value
lookupVar var stack = msum $ map (lookup var) stack
