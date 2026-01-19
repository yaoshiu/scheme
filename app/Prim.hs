{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Prim (primEnv) where

import Control.Monad.Reader (MonadIO (..), MonadReader (..))
import Data.IORef (newIORef)
import qualified Data.Map as Map
import Data.Text (Text)
import Eval (currying, dispatch, eval, ternary, unary)
import SExpr (Env (..), Eval, Op (..), SExpr (..))

mkOp :: (SExpr -> Eval SExpr) -> SExpr
mkOp = SOp . Op

opOnPair :: (SExpr -> SExpr -> Eval SExpr) -> SExpr
opOnPair f = mkOp $ \args -> case args of
  SPair l r -> f l r
  _ -> pure SNil

mapP :: (SExpr -> Eval SExpr) -> SExpr -> Eval SExpr
mapP _ SNil = pure SNil
mapP op (SPair l r) = do
  l' <- op l
  r' <- mapP op r
  pure $ SPair l' r'
mapP op pair =
  dispatch pair $
    fold
      [ SNil,
        opOnPair $ \l r -> do
          l' <- op l
          r' <- mapP op r
          pure $ SPair l' r'
      ]

wrap :: SExpr -> SExpr
wrap op =
  mkOp
    ( \args -> do
        args' <- mapP eval args
        dispatch op args'
    )

fold :: [SExpr] -> SExpr
fold = foldr SPair SNil

bindAndEval :: SExpr -> SExpr -> SExpr -> Eval SExpr
bindAndEval params args expr = do
  env <- ask
  dispatch params $
    fold
      [ fold
          [ mkOp $ \_ -> do
              _ <-
                dispatch params $
                  fold
                    [ fold [mkOp $ currying 1 $ unary $ \ast -> pure ast, args],
                      SNil
                    ]
              eval expr
          ],
        opOnPair $ \p ps ->
          dispatch args $
            fold
              [ mkOp $ \as -> local (const env) $ bindAndEval params as expr,
                opOnPair $ \a as -> do
                  _ <-
                    dispatch p $
                      fold
                        [ fold
                            [mkOp $ currying 1 $ unary $ \ast -> pure ast, a]
                        ]
                  bindAndEval ps as expr
              ]
      ]

vau :: SExpr -> SExpr -> SExpr -> Eval SExpr
vau params envParam expr = do
  parent <- ask
  pure $ mkOp $ \args -> do
    frame <- liftIO $ newIORef $ Map.empty
    dynEnv <- ask
    let env = Env (Just parent) frame
    let envOp = wrap $
          mkOp $
            currying 1 $
              unary $
                \ast -> do
                  local (const dynEnv) $ eval ast
    local (const env) $ do
      _ <- dispatch envParam envOp
      bindAndEval params args expr

car :: SExpr -> Eval SExpr
car (SPair l _) = pure l
car SNil = pure SNil
car pair =
  dispatch pair $
    fold [SNil, opOnPair $ \l _ -> pure l]

cdr :: SExpr -> Eval SExpr
cdr (SPair _ r) = pure r
cdr SNil = pure SNil
cdr pair =
  dispatch pair $
    fold [SNil, opOnPair $ \_ r -> pure r]

primitives :: [(Text, SExpr)]
primitives =
  [ ("quote", mkOp $ currying 1 $ unary $ \ast -> pure ast),
    ("true", SBool True),
    ("false", SBool False),
    ("car", wrap $ mkOp $ currying 1 $ unary car),
    ("cdr", wrap $ mkOp $ currying 1 $ unary cdr),
    ("$", mkOp $ currying 3 $ ternary vau),
    ("wrap", wrap $ mkOp $ currying 1 $ unary $ pure . wrap)
  ]

primEnv :: IO Env
primEnv = do
  entries <- traverse makeEntry primitives
  frame <- newIORef $ Map.fromList entries
  pure $ Env {parent = Nothing, frame}
  where
    makeEntry (name, val) = do
      cell <- newIORef val
      pure (name, cell)
