import Control.Monad
import System.Environment

-- lookup the commands an fold ap into the command list to
-- compute a result.
main :: IO ()
main = do let fns = [ ("double",(2*)),      ("halve",(`div`2)),
                      ("square",(\x->x*x)), ("negate", negate),
                      ("incr",(+1)),        ("decr",(+(-1)))
                    ]
          args <- getArgs
          let val = read (args!!0)
              cmds = map ((flip lookup) fns) (words (args!!1))
          print $ foldl (flip ap) (Just val) cmds
