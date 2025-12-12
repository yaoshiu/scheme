{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module LispVal (LispVal (..), Eval (..), IFunc (..), EnvCtx (..), showVal) where

import Control.Exception
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT (ReaderT))
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T

data LispVal
  = Atom Text
  | List [LispVal]
  | Number Integer
  | String Text
  | Fun IFunc
  | Lambda IFunc EnvCtx
  | Nil
  | Bool Bool
  deriving (Eq)

newtype IFunc = IFunc {fn :: [LispVal] -> Eval LispVal}

instance Eq IFunc where
  _ == _ = False

type EnvCtx = Map.Map Text LispVal

newtype Eval a = Eval {unEval :: ReaderT EnvCtx IO a}
  deriving
    ( Monad,
      Functor,
      Applicative,
      MonadReader EnvCtx,
      MonadIO
    )

showVal :: LispVal -> Text
showVal val =
  case val of
    (Atom atom) -> atom
    (String str) -> T.concat ["\"", str, "\""]
    (Number num) -> T.show num
    (Bool True) -> "#t"
    (Bool False) -> "#f"
    Nil -> "Nil"
    (List contents) -> T.concat ["(", T.unwords $ showVal <$> contents, ")"]
    (Fun _) -> "(internal function)"
    (Lambda _ _) -> "(lambda function)"
