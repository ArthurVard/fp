{-# LANGUAGE TupleSections #-}
module FP.Interpreter (runProgram) where

import           FP.AST
import           FP.Value
import           FP.Env                 (Interp(..), FunDef(..))
import qualified FP.Env                 as Env

import           Control.Applicative    ((<$>), pure)
import           Control.Monad          (forM, forM_, foldM)
import           Control.Monad.Identity (runIdentity)
import           Control.Monad.State    (evalStateT)
import           Prelude hiding(exp)

interpFunApp :: Function -> Object -> Interp Object
interpFunApp (Function symbol) o = do
    funDef <- Env.lookup symbol
    case funDef of
        FunDef fun                  -> interpFunApp fun o
        PreludeFunDef preludeFun    -> return $ preludeFun o

interpFunApp (Composition f g) o = do
    o' <- interpFunApp g o
    interpFunApp f o'

interpFunApp (Condition p f g) o = do
    cond <- interpFunApp p o
    case cond of
        AtomObject (BoolAtom True)  -> interpFunApp f o
        AtomObject (BoolAtom False) -> interpFunApp g o
        _                           -> return Bottom

interpFunApp while@(While p f) o = do
    cond <- interpFunApp p o
    case cond of
        AtomObject (BoolAtom True)  -> do
            o' <- interpFunApp f o
            interpFunApp while o'
        AtomObject (BoolAtom False) -> return o
        _                           -> return Bottom

interpFunApp (Constant c) o =
    case o of
        Bottom  -> return Bottom
        _       -> return c

interpFunApp (ApplyToAll f) o =
    case o of
        SequenceObject os   -> SequenceObject <$> (forM os $ interpFunApp f)
        _                   -> return Bottom

interpFunApp (BinaryToUnary f a) o = interpFunApp f $ SequenceObject [a, o]

interpFunApp (Construction fs) o = do
    os <- forM fs (`interpFunApp` o)
    return $ if Bottom `elem` os
        then Bottom
        else SequenceObject os

interpFunApp (Insert f) o =
    case o of
        SequenceObject (x:xs)   -> foldM f' x xs
        _                       -> return Bottom
    where f' a b = interpFunApp f $ SequenceObject [a, b]

interpExp :: Expression -> Interp Object
interpExp exp = case exp of
    Object      o   -> pure o
    Application f o -> interpFunApp f o

interpProgram :: Program -> Interp Object
interpProgram (Program defs exp) = do
    forM_ defs (\(Definition funSymbol fun) -> Env.extend funSymbol $ FunDef fun)
    interpExp exp

runProgram :: Program -> Object
runProgram = runIdentity . flip evalStateT Env.preludeDefs . runInterp . interpProgram
