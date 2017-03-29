import Control.Monad.Error
import Data.Char

-- This is the type of our parse error representation.
data ParseError = Err {location :: Int, reason:: String}

-- We make it an instance of the Error class
instance Error ParseError where
  noMsg    = Err 0 "Parse Error"
  strMsg s = Err 0 s

type ParseMonad = Either ParseError

-- parseHexDigit attempts to convert a single hex digit into
-- an Integer in the ParseMonad monad and throws an error on an
-- invalid character
parseHexDigit :: Char -> Int -> ParseMonad Integer
parseHexDigit c idx = if isHexDigit c then
                        return (toInteger (digitToInt c))
                      else
                        throwError (Err idx ("Invalid character '" ++ [c] ++ "'"))

-- parseHex parses a string containing a hexadecimal number into
-- an Integer in the ParseMonad monad. A parse erro from parseHexDigit
-- will cause an exceptional return from parseHex.
parseHex :: String -> ParseMonad Integer
parseHex s = parseHex' s 0 1
  where parseHex' []     val _   = return val
        parseHex' (c:cs) val idx = do d <- parseHexDigit c idx
                                      parseHex' cs ((val * 16)+d) (idx+1)

-- toString converts an Integer into a Strng in the ParseMonad monad
toString :: Integer -> ParseMonad String
toString n = return $ show n

-- convert takes a String containing a hexadecimal representation of
-- a number to a String containing a decimal representation of that
-- number. a parse error on the input String will generate a
-- descriptive error message as the ouput String.
convert :: String -> String
convert s = let (Right str) = do {n <- parseHex s; toString n} `catchError` printError
            in str
  where printError e = return $ "At index " ++ (show (location e)) ++ ":" ++ (reason e)
