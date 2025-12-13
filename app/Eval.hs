{-# LANGUAGE OverloadedStrings #-}

module Eval where

import Control.Monad.Reader
import LispVal
import qualified Data.Map as Map
import Data.Text (Text)
import Parser
import Control.Exception
import qualified Data.Text as T

basicEnv :: Map.Map Text LispVal
basicEnv = Map.fromList $ primEnv <> [("read", Fun $ IFunc $ unop $ readFn)]

evalFile :: Text -> IO ()
evalFile fileExpr = (runASTinEnv basicEnv $ fileToEvalForm fileExpr) >>= print

fileToEvalForm :: Text -> Eval LispVal
fileToEvalForm input = either (throw . PError . show) evalBody $ readExprFile input

runParseTest :: Text -> Text
runParseTest input = either (T.pack . show) (T.pack . show) $ readExpr input

runASTinEnv :: EnvCtx -> Eval b -> IO b
runASTinEnv code action = runResourceT $ runReaderT (unEval action) code

eval :: LispVal -> Eval LispVal
eval (List [Atom "quote", val]) = return val

eval (Number i) = pure $ Number i
eval (String s) = pure $ String s
eval (Bool b) = pure $ Bool b
eval (List []) = pure Nil
eval Nil = pure Nil

eval (List [Atom "write", rest]) = pure . String . T.pack $ show rest
eval (List (Atom "write" : rest)) = pure . String . T.pack . show $ List rest
eval n@(Atom _) = getVar n
eval (List [Atom "if", pred, truExpr, flsExpr]) = do
  ifRes <- eval pred
  case ifRes of
    (Bool True) -> eval truExpr
    (Bool False) -> eval flsExpr
    _ -> throw $ BadSpecialForm "if"

getVar :: LispVal -> Eval LispVal
getVar (Atom atom) = do
  env <- ask
  case Map.lookup atom env of
    Just x -> pure x
    Nothing -> throw $ UnboundVar atom

eval (List [Atom "let", List pairs, expr]) = do
  env <- ask
  atoms <- mapM ensureAtom $ getEven pairs
  vals <- mapM eval $ getOdd pairs
  let env' = Map.fromList (zipWith (\a b -> (extractVar a, b)) atoms vals) <> env
  in local (const env') $ evalBody expr


