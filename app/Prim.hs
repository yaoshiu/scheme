{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Prim where

import Control.Monad.Except (throwError)
import Data.IORef (newIORef)
import qualified Data.Map as Map
import Data.Text (Text)
import Eval (Env (..), Eval, EvalError (..), Value (..), showVal)

unpackNum :: Value -> Eval Integer
unpackNum (VNumber n) = pure n
unpackNum v = throwError $ TypeError $ "expected number, got " <> showVal v

unpackBool :: Value -> Eval Bool
unpackBool (VBoolean b) = pure b
unpackBool v = throwError $ TypeError $ "expected boolean, got " <> showVal v

mkNumOp :: Integer -> (Integer -> Integer -> Integer) -> [Value] -> Eval Value
mkNumOp indent op args = do
  nums <- mapM unpackNum args
  pure $ VNumber $ case nums of
    [] -> indent
    [x] -> indent `op` x
    x : xs -> foldl' op x xs

divOp :: [Value] -> Eval Value
divOp args = do
  nums <- mapM unpackNum args
  case nums of
    [] -> pure $ VNumber 1
    [x] ->
      if x == 0
        then throwError $ NumericError "division by zero"
        else pure $ VNumber $ 1 `div` x
    x : xs -> do
      if any (== 0) xs
        then throwError $ NumericError "division by zero"
        else pure $ VNumber $ foldl' div x xs

mkCmpOp :: (Integer -> Integer -> Bool) -> [Value] -> Eval Value
mkCmpOp op args = do
  nums <- mapM unpackNum args
  pure $ VBoolean $ check nums
  where
    check [] = True
    check [_] = True
    check (x : y : zs) = op x y && check (y : zs)

arityError :: Int -> [Value] -> Eval Value
arityError expected args = throwError $ ArityError expected $ length args

cons :: [Value] -> Eval Value
cons [a, b] = pure $ VPair a b
cons args = arityError 2 args

car :: [Value] -> Eval Value
car [VPair a _] = pure a
car [v] = throwError $ TypeError $ "expected a pair, got: " <> showVal v
car args = arityError 1 args

cdr :: [Value] -> Eval Value
cdr [VPair _ b] = pure b
cdr [v] = throwError $ TypeError $ "expected a pair, got: " <> showVal v
cdr args = arityError 1 args

list :: [Value] -> Eval Value
list args = pure $ foldr VPair VNil args

isNull :: [Value] -> Eval Value
isNull [v] = pure $ VBoolean $ case v of VNil -> True; _ -> False
isNull args = arityError 1 args

primitives :: [(Text, [Value] -> Eval Value)]
primitives =
  [ ("+", mkNumOp 0 (+)),
    ("-", mkNumOp 0 (-)),
    ("*", mkNumOp 1 (*)),
    ("/", divOp),
    ("=", mkCmpOp (==)),
    ("<", mkCmpOp (<)),
    (">", mkCmpOp (>)),
    ("<=", mkCmpOp (<=)),
    (">=", mkCmpOp (>=)),
    ("cons", cons),
    ("car", car),
    ("cdr", cdr)
  ]

primEnv :: IO Env
primEnv = do
  entries <- mapM makeEntry primitives
  bindings <- newIORef $ Map.fromList entries
  pure $ Env {parent = Nothing, bindings}
  where
    makeEntry (name, func) = do
      cell <- newIORef (VPrim func)
      pure (name, cell)
