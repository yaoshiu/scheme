{-# LANGUAGE OverloadedStrings #-}

module Prim where
import Eval (Value (..), Eval, EvalError (..), showVal)
import Control.Monad.Except (throwError)

unpackNum :: Value -> Eval Integer
unpackNum (VNumber n) = pure n
unpackNum v = throwError $ TypeError $ "expected number, got " <> showVal v

unpackBool :: Value -> Eval Bool
unpackBool (VBoolean b) = pure b
unpackBool v = throwError $ TypeError $ "expected boolean, got " <> showVal v

numFoldOp :: (Integer -> Integer -> Integer) -> Integer -> [Value] -> Eval Value
numFoldOp op acc args = do
  nums <- mapM unpackNum args
  pure $ VNumber $ foldl' op acc nums 
