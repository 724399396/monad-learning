-- this is the format of our log entries
data Entry = Log {timestamp::ClockTime, msg:: String} deriving Eq

instance Show Entry where
  show (Log t s) = (show t) ++ " | " ++ s

-- this is the combined monad type
type LogWriter a = WriterT [Entry] IO a

-- add a message to the log
logMsg :: String -> LogWriter ()
logMsg s = do t <- liftIO getClockTime
              tell [Log t s]

-- this handles one package
filterOne :: [Rule] -> Packet -> LogWriter (Maybe Packet)

