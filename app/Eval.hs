{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Eval
  ( eval,
    runEval,
    renderVal,
    currying,
    unary,
    binary,
    ternary,
    dispatch,
  )
where

import Control.Monad.Cont (ContT (..))
import Control.Monad.Reader (MonadIO (..), MonadReader (..), ReaderT (..))
import Data.Char (ord)
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace (trace)
import SExpr (Env (..), Eval (..), Op (..), SExpr (..), fold, mkOp, opOnPair, unicodeSize)

runEval :: Env -> Eval SExpr -> IO SExpr
runEval env ev =
  runContT
    (runReaderT (unEval ev) env)
    pure

asString :: SExpr -> Maybe String
asString SNil = Just ""
asString (SPair (SChar c) rest) =
  case asString rest of
    Just s -> Just (c : s)
    Nothing -> Nothing
asString _ = Nothing

renderVal :: Bool -> SExpr -> Text
renderVal _ (SNum n) = T.show n
renderVal _ SNil = "()"
renderVal _ (SBool True) = "true"
renderVal _ (SBool False) = "false"
renderVal render (SSym s) = (if render then "'" else "") <> s
renderVal _ (SOp {}) = "#<operative>"
renderVal render lst@(SPair l r) =
  case asString lst of
    Just s -> if render then T.show s else T.pack s
    Nothing ->
      let showTail SNil = ""
          showTail (SPair left right) =
            " " <> renderVal render left <> showTail right
          showTail right = " . " <> renderVal render right
       in "(" <> renderVal render l <> showTail r <> ")"
renderVal True (SChar c) = case c of
  ' ' -> "#\\w"
  '\n' -> "#\\n"
  '\t' -> "#\\t"
  _ -> T.pack $ '#' : [c]
renderVal False (SChar c) = T.pack [c]

define :: Text -> SExpr -> Eval SExpr
define name val = do
  Env {frame} <- ask
  cell <- liftIO $ newIORef val
  liftIO $ modifyIORef' frame (Map.insert name cell)
  pure val

currying :: Int -> ([SExpr] -> Eval SExpr) -> SExpr -> Eval SExpr
currying n handler stream = collect [] n stream
  where
    collect acc 0 _ = handler (reverse acc)
    collect acc needed SNil = pure $ SOp $ Op $ \more -> collect acc needed more
    collect acc needed (SPair h t) = collect (h : acc) (needed - 1) t
    collect acc needed pair =
      dispatch pair $
        fold
          [ mkOp $ \more -> collect acc needed more,
            opOnPair $ \h t -> collect (h : acc) (needed - 1) t
          ]

unary :: (SExpr -> Eval SExpr) -> [SExpr] -> Eval SExpr
unary f [x] = f x
unary _ _ = error "Bug: unary called with wrong number of args"

binary :: (SExpr -> SExpr -> Eval SExpr) -> [SExpr] -> Eval SExpr
binary f [x, y] = f x y
binary _ _ = error "Bug: binary called with wrong number of args"

ternary :: (SExpr -> SExpr -> SExpr -> Eval SExpr) -> [SExpr] -> Eval SExpr
ternary f [x, y, z] = f x y z
ternary _ _ = error "Bug: ternary called with wrong number of args"

dispatch :: SExpr -> SExpr -> Eval SExpr
dispatch SNil args = currying 2 (binary $ \t _ -> eval t) args
dispatch (SBool True) args = dispatch SNil args
dispatch (SBool False) args = currying 2 (binary $ \_ f -> eval f) args
dispatch (SSym name) args =
  currying
    2
    ( binary $
        \expr _ -> do
          val <- eval expr
          define name val
    )
    args
dispatch (SOp (Op op)) args = op args
dispatch (SNum num) args =
  currying
    3
    ( ternary $ \z p n ->
        case compare num 0 of
          EQ -> eval z
          GT -> do
            op <- eval p
            dispatch op $ fold [SNum (num - 1)]
          LT -> do
            op <- eval n
            dispatch op $ fold [SNum (abs num - 1)]
    )
    args
dispatch p@(SPair {}) args =
  currying
    2
    ( binary $ \_ f -> do
        op <- eval f
        dispatch op p
    )
    args
dispatch (SChar c) args =
  currying
    unicodeSize
    (\collectedArgs -> let target = collectedArgs !! ord c in eval target)
    args

eval :: SExpr -> Eval SExpr
eval sym@(SSym name) = do
  Env {parent, frame} <- ask
  bindings <- liftIO $ readIORef $ frame
  case Map.lookup name bindings of
    Just val -> liftIO $ readIORef val
    Nothing -> case parent of
      Just p -> local (const p) $ eval sym
      Nothing -> pure SNil
eval (SPair op args) = do
  op' <- eval op
  dispatch op' args
eval x = pure $ x
