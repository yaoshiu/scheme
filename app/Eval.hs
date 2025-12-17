{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Eval where

import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.Reader (MonadIO (..), MonadReader (..), ReaderT (..))
import Data.IORef (IORef, newIORef, readIORef,  modifyIORef')
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Parser (SExpr (..))
import Control.Monad (foldM)

data Value
  = VNumber Integer
  | VBoolean Bool
  | VString Text
  | VSymbol Text
  | VPrim ([Value] -> Eval Value)
  | VFunc [Text] SExpr Env
  | VPair Value Value
  | VNil
  | VVoid

data EvalError
  = UnboundVariable Text
  | TypeError Text
  | ArityError Int Int
  | SyntaxError Text

type Cell = IORef Value

type Frame = IORef (Map.Map Text Cell)

data Env = Env {parent :: Maybe Env, bindings :: Frame}

newtype Eval a = Eval {unEval :: ReaderT Env (ExceptT EvalError IO) a}
  deriving
    ( Monad,
      Functor,
      Applicative,
      MonadReader Env,
      MonadIO,
      MonadError EvalError
    )

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
      SyntaxError m -> "syntax error: " <> m

runEval :: Env -> Eval a -> IO (Either EvalError a)
runEval env ev = runExceptT (runReaderT (unEval ev) env)

showSExpr :: SExpr -> Text
showSExpr (PBoolean False) = "#f"
showSExpr (PBoolean True) = "#t"
showSExpr (PDotted xs t) = "(" <> T.unwords (map showSExpr xs) <> " . " <> showSExpr t <> ")"
showSExpr (PList xs) = "(" <> T.unwords (map showSExpr xs) <> ")"
showSExpr (PNumber n) = T.show n
showSExpr (PString s) = "\"" <> s <> "\""
showSExpr (PSymbol s) = s

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
      showTail (VPair left right) = " " <> showVal left <> showTail right
      showTail right = " . " <> showVal right
   in "(" <> showVal l <> showTail r <> ")"
showVal VVoid = "#<void>"

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
eval (PList (PSymbol "begin" : body)) = evalBegin body
eval (PList [PSymbol "define", PSymbol name, expr]) = eval expr >>= evalDefine name
eval (PList (f : args)) = do
  func <- eval f
  values <- mapM eval args
  apply func values
eval x = pure $ datumToValue x

evalBegin :: [SExpr] -> Eval Value
evalBegin body = foldM (const eval) VVoid body

evalDefine :: Text -> Value -> Eval Value
evalDefine name val = do
  Env {bindings} <- ask
  cell <- liftIO $ newIORef val
  liftIO $ modifyIORef' bindings $ Map.insert name cell
  pure VVoid

evalLet :: [SExpr] -> SExpr -> Eval Value
evalLet bindings body = do
  pairs <- mapM parseBinding bindings
  let (names, exprs) = unzip pairs
  eval $
    PList (PList [PSymbol "lambda", PList (map PSymbol names), body] : exprs)

parseBinding :: SExpr -> Eval (Text, SExpr)
parseBinding (PList [PSymbol name, expr]) = pure (name, expr)
parseBinding bad =
  throwError $ SyntaxError $ "invalid let binding: " <> showSExpr bad

evalLambda :: [SExpr] -> SExpr -> Eval Value
evalLambda params body = do
  names <- mapM getParam params
  env <- ask
  pure (VFunc names body env)

getParam :: SExpr -> Eval Text
getParam (PSymbol s) = pure s
getParam bad = throwError $ SyntaxError $ "invalid parameter: " <> showSExpr bad

apply :: Value -> [Value] -> Eval Value
apply (VPrim f) args = f args
apply (VFunc params body env) args
  | length params /= length args =
      throwError $ ArityError (length params) (length args)
  | otherwise = do
      args' <- mapM (liftIO . newIORef) args
      bindings <- liftIO $ newIORef $ Map.fromList $ zip params args'
      let env' = Env {parent = Just env, bindings }
      local (const env') $ eval body
apply v _ = throwError $ TypeError $ "not a function: " <> showVal v

getVar :: Text -> Eval Value
getVar s = do
  env <- ask
  cell <- liftIO $ lookFor s env
  maybe
    (throwError $ UnboundVariable s)
    (liftIO . readIORef)
    cell
  where
    lookFor :: Text -> Env -> IO (Maybe Cell)
    lookFor name Env {parent, bindings} = do
      frame <- readIORef bindings
      maybe
        (maybe (pure Nothing) (lookFor name) parent)
        (pure . Just)
        $ Map.lookup name frame
