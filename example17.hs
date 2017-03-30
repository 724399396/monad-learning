
-- this is the format of our log entries
data Entry = Log {count::Int, msg:: String} deriving Eq

-- add a message to the log
logMsg :: String -> Write [Entry] ()
logMsg s = tell [Log 1 s]

-- this handles one packet
filterOne :: [Rule] -> Packet -> Writer [Entry] (Maybe Packet)
filterOne rules packet = do
  rule <- return (match rules packet)
  case rule of
    Nothing -> do
      logMsg $ "DROPPING UNMATCHED PACKET: " ++ (show packet)
      return Nothing
    (Just r) -> do
      when (logIt r) $ logMsg ("MATCH: " ++ (show r) ++ " <=> " ++ (show packet))
      case r of (Rule Accept _ _) -> return $ Just packet
                (Rule Reject _ _) -> return Nothing

-- merge identical entries at the end of the log
-- This function uses [Entry] as both the log type and the result type.
-- When two identical messages are merged, the result is just the message
-- with an incremented count. When two different messages are merged,
-- the first message is logged and the second is returned as the result.
mergeEntries :: [Entry] -> [Entry] -> Writer [Entry] [Entry]
mergeEntries [] x = return x
mergeEntries x [] = return x
mergeEntries [e1] [e2] = let (Log n msg) = e1
                             (Log n' msg') = e2
                         in if msg == msg' then
                                return [(Log (n+n') msg)]
                              else
                                do tell [e1]
                                   return e2

-- This is a compex-looing function but it is actually pretty simple.
-- It maps a function over a list of values to get a list of Writers,
-- then runs each writer and cmobines the results. The result of the function
-- is a writer whose value is a list of all the values from the writers and whose
-- log output is the result of folding the merge operator into the individual
-- log entries (using 'initial' as the inital log value).
groupSame :: (Monoid a) => a -> (a -> a -> Writer a a) -> [b] -> (b -> Writer a c) -> Writer a [c]
groupSame initial merge [] = do tell inital
                                return []
groupSame initial merge (x:xs) fn = do (result, output) <- return (runWriter (fn x)
                                       new              <- merge inital output
                                       rest             <- groupSame new merge xs fn
                                       return (result:rest)

-- this filters a list of packets, producing a filtered list an a log of
-- the activity in which consecutive message are merged
filterAll :: [Rule] -> [Package] -> Writer [Entry] [Package]
filterAll rules packages = do tell [Log 1 "STARTING PACKET FILTER"]
                              out <- groupSame [] mergeEntries packets (filterOne rules)
                              tell [Log 1 "STOPPING PACKET FILTER"]
                              return (catMaybes out)
