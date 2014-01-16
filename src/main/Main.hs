module Main where

import FP.Parser
import FP.Interpreter

import Control.Applicative
import Data.Monoid
import Options.Applicative

data Option = Option { filename :: String }

runWithOptions :: Option -> IO ()
runWithOptions opts = do
    source <- readFile $ filename opts
    case parseFP source of
        Left e -> print e
        Right program -> do
            print $ runProgram program

main :: IO ()
main =  execParser opts >>= runWithOptions
    where parser = Option <$> argument str (metavar "FILE")
          opts = info parser mempty
