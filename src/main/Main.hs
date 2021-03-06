module Main where

import FP.Parser
import FP.Interpreter

import Control.Applicative
import Control.Monad
import Data.Monoid
import Options.Applicative

data Option = Option { filename :: String, verbose :: Bool }

runWithOptions :: Option -> IO ()
runWithOptions opts = do
    source <- readFile $ filename opts
    case parseFP source of
        Left e -> print e
        Right program -> do
            when (verbose opts == True) $
                print program
            print $ runProgram program

main :: IO ()
main =  execParser opts >>= runWithOptions
    where parser = Option <$> argument str (metavar "FILE")
                          <*> switch (short 'v' <> long "verbose" <> help "Verbose mode")
          opts = info parser mempty
