{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Eval
  ( Value (..),
    EvalError (..),
    Env (..),
    Eval (..),
    eval,
    runEval,
    apply,
    showVal,
    printError,
  )
where

import Control.Monad (foldM)
import Control.Monad.Cont (ContT (..), MonadCont)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.Reader (MonadIO (..), MonadReader (..), ReaderT (..))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Parser (SExpr (..))

data Value
  = VNumber Integer
  | VBoolean Bool
  | VString Text
  | VSymbol Text
  | VPrim ([Value] -> Eval Value)
  | VCont (Value -> Eval Value)
  | VFunc [Text] SExpr Env
  | VPair Value Value
  | VNil

data EvalError
  = UnboundVariable Text
  | TypeError Text
  | ArityError Int Int
  | SyntaxError Text
  | NumericError Text

type EvalResult = Either EvalError Value

type Cell = IORef Value

type Frame = IORef (Map.Map Text Cell)

data Env = Env {parent :: Maybe Env, bindings :: Frame}

newtype Eval a = Eval {unEval :: ReaderT Env (ExceptT EvalError (ContT EvalResult IO)) a}
  deriving
    ( Monad,
      Functor,
      Applicative,
      MonadReader Env,
      MonadIO,
      MonadError EvalError,
      MonadCont
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
      NumericError m -> "numeric error: " <> m

runEval :: Env -> Eval Value -> IO (Either EvalError Value)
runEval env ev =
  runContT
    (runExceptT (runReaderT (unEval ev) env))
    pure

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
showVal (VCont _) = "#<continuation>"

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
eval (PSymbol s) = getVar s >>= liftIO . readIORef
eval (PList [PSymbol "quote", x]) = pure $ datumToValue x
eval (PList [PSymbol "lambda", PList params, expr]) = evalLambda params expr
eval (PList [PSymbol "let", PList bindings, expr]) = evalLet bindings expr
eval (PList (PSymbol "do" : body)) = evalDo body
eval (PList [PSymbol "define!", PSymbol name, expr]) = eval expr >>= evalDefine name
eval (PList [PSymbol "define!", PList (PSymbol name : params), expr]) =
  eval $
    PList
      [ PSymbol "define!",
        PSymbol name,
        PList [PSymbol "lambda", PList params, expr]
      ]
eval (PList [PSymbol "set!", PSymbol name, expr]) = eval expr >>= evalSet name
eval (PList [PSymbol "if", pre, con, alt]) = evalIf pre con alt
eval (PList (f : args)) = do
  func <- eval f
  values <- mapM eval args
  apply func values
eval x = pure $ datumToValue x

evalIf :: SExpr -> SExpr -> SExpr -> Eval Value
evalIf pre con alt = do
  res <- eval pre
  case res of
    VBoolean False -> eval alt
    _ -> eval con

evalSet :: Text -> Value -> Eval Value
evalSet name val = do
  cell <- getVar name
  liftIO $ modifyIORef' cell $ const val
  pure val

evalDo :: [SExpr] -> Eval Value
evalDo body = foldM (const eval) VNil body

evalDefine :: Text -> Value -> Eval Value
evalDefine name val = do
  Env {bindings} <- ask
  cell <- liftIO $ newIORef val
  liftIO $ modifyIORef' bindings $ Map.insert name cell
  pure val

evalLet :: [SExpr] -> SExpr -> Eval Value
evalLet bindings expr = do
  pairs <- mapM parseBinding bindings
  let (names, vals) = unzip pairs
  eval $
    PList (PList [PSymbol "lambda", PList (map PSymbol names), expr] : vals)

parseBinding :: SExpr -> Eval (Text, SExpr)
parseBinding (PList [PSymbol name, expr]) = pure (name, expr)
parseBinding bad =
  throwError $ SyntaxError $ "invalid let binding: " <> showSExpr bad

evalLambda :: [SExpr] -> SExpr -> Eval Value
evalLambda params expr = do
  names <- mapM getParam params
  env <- ask
  pure (VFunc names expr env)

getParam :: SExpr -> Eval Text
getParam (PSymbol s) = pure s
getParam bad = throwError $ SyntaxError $ "invalid parameter: " <> showSExpr bad

apply :: Value -> [Value] -> Eval Value
apply (VPrim f) args = f args
apply (VFunc params expr env) args
  | length params /= length args =
      throwError $ ArityError (length params) (length args)
  | otherwise = do
      args' <- mapM (liftIO . newIORef) args
      bindings <- liftIO $ newIORef $ Map.fromList $ zip params args'
      let env' = Env {parent = Just env, bindings}
      local (const env') $ eval expr
apply (VCont k) [val] = k val
apply (VCont _) args = throwError $ ArityError 1 $ length args
apply v _ = throwError $ TypeError $ "not a function: " <> showVal v

getVar :: Text -> Eval Cell
getVar s = do
  env <- ask
  cell <- liftIO $ lookFor s env
  maybe
    (throwError $ UnboundVariable s)
    pure
    cell
  where
    lookFor :: Text -> Env -> IO (Maybe Cell)
    lookFor name Env {parent, bindings} = do
      frame <- readIORef bindings
      maybe
        (maybe (pure Nothing) (lookFor name) parent)
        (pure . Just)
        $ Map.lookup name frame
