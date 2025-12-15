{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Eval where

import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT (runReaderT), ask, local)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Trace (trace)
import Parser (SExpr (..))

data Value
  = VNumber Integer
  | VBoolean Bool
  | VString Text
  | VSymbol Text
  | VPrim ([Value] -> Eval Value)
  | VFunc [Text] SExpr EnvCtx
  | VPair Value Value
  | VNil

data EvalError
  = UnboundVariable Text
  | TypeError Text
  | ArityError Int Int

newtype Eval a = Eval {unEval :: ReaderT EnvCtx (ExceptT EvalError IO) a}
  deriving
    ( Monad,
      Functor,
      Applicative,
      MonadReader EnvCtx,
      MonadIO,
      MonadError EvalError
    )

type EnvCtx = Map.Map Text Value

printError :: EvalError -> IO ()
printError err =
  TIO.putStrLn $
    case err of
      UnboundVariable s -> "unbound variable: " <> s
      TypeError m -> "type error: " <> m
      ArityError expected got ->
        "arity mismatch: expected "
          <> T.show expected
          <> " argument(s), but got "
          <> T.show got

runEval :: EnvCtx -> Eval a -> IO (Either EvalError a)
runEval env ev = runExceptT (runReaderT (unEval ev) env)

showVal :: Value -> Text
showVal (VNumber n) = T.pack $ show n
showVal (VString s) = "\"" <> s <> "\""
showVal VNil = "()"
showVal (VBoolean True) = "#t"
showVal (VBoolean False) = "#f"
showVal (VSymbol s) = s
showVal (VPrim _) = "#<primitive>"
showVal (VFunc _ _ _) = "#<procedure>"
showVal (VPair l r) =
  let showTail VNil = ""
      showTail (VPair l r) = " " <> showVal l <> showTail r
      showTail r = " . " <> showVal r
   in "(" <> showVal l <> showTail r <> ")"

datumToValue :: SExpr -> Value
datumToValue (PNumber n) = VNumber n
datumToValue (PBoolean b) = VBoolean b
datumToValue (PString s) = VString s
datumToValue (PSymbol s) = VSymbol s
datumToValue (PList xs) = foldr VPair VNil (map datumToValue xs)
datumToValue (PDotted xs t) =
  foldr VPair (datumToValue t) (map datumToValue xs)

eval :: SExpr -> Eval Value
eval (PList []) = pure VNil
eval (PSymbol s) = getVar s
eval (PList [PSymbol "quote", x]) = pure $ datumToValue x
eval (PList [PSymbol "lambda", PList params, body]) = evalLambda params body
eval (PList [PSymbol "let", PList bindings, body]) = evalLet bindings body
eval (PList (f : args)) = do
  func <- eval f
  values <- mapM eval args
  apply func values
eval x = pure $ datumToValue x

evalLet :: [SExpr] -> SExpr -> Eval Value
evalLet bindings body = do
  pairs <- mapM parseBinding bindings
  let (names, exprs) = unzip pairs
  eval $
    PList (PList [PSymbol "lambda", PList (map PSymbol names), body] : exprs)

parseBinding :: SExpr -> Eval (Text, SExpr)
parseBinding (PList [PSymbol name, expr]) = pure (name, expr)
parseBinding bad =
  throwError
    ( TypeError
        ("invalid let binding: " <> T.show bad)
    )

evalLambda :: [SExpr] -> SExpr -> Eval Value
evalLambda params body = do
  names <- mapM getParam params
  env <- ask
  pure (VFunc names body env)

getParam :: SExpr -> Eval Text
getParam (PSymbol s) = pure s
getParam bad = throwError (TypeError ("invalid parameter: " <> T.show bad))

apply :: Value -> [Value] -> Eval Value
apply (VPrim f) args = f args
apply (VFunc params body closureEnv) args
  | length params /= length args =
      throwError $ ArityError (length params) (length args)
  | otherwise = do
      let newEnv = Map.union (Map.fromList (zip params args)) closureEnv
      local (const newEnv) $ eval body

getVar :: Text -> Eval Value
getVar s = do
  env <- ask
  case Map.lookup s env of
    Just x -> pure x
    Nothing -> throwError $ UnboundVariable s
