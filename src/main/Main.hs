module Main where

import FP.Parser
import FP.Interpreter

main :: IO ()
main =  do
    source <- getContents
    case parseFP source of
        Left e -> print e
        Right program -> do
            print $ runProgram program
