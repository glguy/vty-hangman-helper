module Options where

import Data.List (foldl')
import System.Console.GetOpt
import System.Environment

data Options = Options
  { scrubOption  :: Bool
  , wordlistFile :: FilePath
  }

defaultOptions :: Options
defaultOptions = Options
  { scrubOption  = False
  , wordlistFile = "wordlist1.txt"
  }

setScrub :: Bool -> Options -> Options
setScrub x o = o { scrubOption = x }

setWordlist :: FilePath -> Options -> Options
setWordlist fp o = o { wordlistFile = fp }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['s'] ["scrub"] (NoArg (setScrub True)) "Scrub word"
  , Option ['f'] ["wordlist"] (ReqArg setWordlist "Filename") "Word list"
  ]

parseOptions :: IO (Options, String)
parseOptions = do
  args <- getArgs
  case getOpt Permute options args of
    (optFuns, [w], []  ) -> return (foldl' (\x f -> f x) defaultOptions optFuns, w)
    (_,       [] , []  ) -> fail "Initial word not specified"
    (_,       _  , []  ) -> fail "Too many arguments"
    (_,       _  , errs) -> fail (unlines errs)

