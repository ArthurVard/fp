{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module FP.Env
    (
      Env
    , preludeDefs
    , extend
    , lookup
    , Interp(..)
    , FunDef(..)
    ) where

import           FP.AST
import           FP.Value
import           FP.Function as Function

import           Control.Applicative
import           Control.Monad.Identity
import           Control.Monad.State
import           Data.Map    (Map)
import qualified Data.Map    as Map
import           Prelude     hiding (lookup)

newtype Interp a = Interp { runInterp :: StateT Env Identity a }
    deriving (Functor, Applicative, Monad, MonadState Env)

data FunDef = FunDef Function | PreludeFunDef FunValue

type Env = Map Symbol FunDef

preludeDefs :: Env
preludeDefs  = Map.fromList
                        [("tl",         PreludeFunDef Function.tl),
                         ("tlr",        PreludeFunDef Function.tlr),
                         ("id",         PreludeFunDef Function.id),
                         ("atom",       PreludeFunDef Function.atom),
                         ("eq",         PreludeFunDef Function.eq),
                         ("null",       PreludeFunDef Function.null),
                         ("reverse",    PreludeFunDef Function.reverse),
                         ("distl",      PreludeFunDef Function.distl),
                         ("distr",      PreludeFunDef Function.distr),
                         ("length",     PreludeFunDef Function.length),
                         ("trans",      PreludeFunDef Function.trans),
                         ("and",        PreludeFunDef Function.and),
                         ("or",         PreludeFunDef Function.or),
                         ("not",        PreludeFunDef Function.not),
                         ("apndl",      PreludeFunDef Function.apndl),
                         ("apndr",      PreludeFunDef Function.apndr),
                         ("rotl",       PreludeFunDef Function.rotl),
                         ("rotr",       PreludeFunDef Function.rotr),
                         ("+",          PreludeFunDef Function.add),
                         ("-",          PreludeFunDef Function.subtract),
                         ("*",          PreludeFunDef Function.multiply),
                         ("/",          PreludeFunDef Function.divide)
                        ]

extend :: Symbol -> FunDef -> Interp ()
extend symbol f = do
    env <- get
    put $ Map.insert symbol f env

lookup :: Symbol -> Interp FunDef
lookup symbol = do
    env <- get
    case Map.lookup symbol env of
        Just f -> return f
        Nothing -> error $ "undefined function: " ++ symbol
