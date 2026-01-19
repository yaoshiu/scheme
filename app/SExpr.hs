{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SExpr
  ( SExpr (..),
    Env (..),
    Eval (..),
    Op (..),
    Cell,
    unicodeSize,
    quote
  )
where

import Control.Monad.Cont (ContT, MonadCont)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT)
import Data.IORef (IORef)
import qualified Data.Map as Map
import Data.Text (Text)

unicodeSize :: Int
unicodeSize = 1114112

data SExpr
  = SNil
  | SSym Text
  | SBool Bool
  | SNum Integer
  | SPair SExpr SExpr
  | SOp Op
  | SChar Char
  deriving (Eq, Ord)

newtype Op = Op (SExpr -> Eval SExpr)

instance Eq Op where
  _ == _ = False

instance Ord Op where
  _ <= _ = False

instance Show Op where
  show _ = error "cannot show a function"

type Cell = IORef SExpr

type Frame = IORef (Map.Map Text Cell)

data Env = Env {parent :: Maybe Env, frame :: Frame}

instance Eq Env where
  _ == _ = False

instance Ord Env where
  _ <= _ = False

newtype Eval a = Eval
  { unEval ::
      ReaderT Env (ContT SExpr IO) a
  }
  deriving
    ( Monad,
      Functor,
      Applicative,
      MonadReader Env,
      MonadIO,
      MonadCont
    )

quote :: SExpr
quote = SOp $ Op $ \ast -> pure $ case ast of
    SNil -> quote
    SPair l _ -> l
    _ -> ast
