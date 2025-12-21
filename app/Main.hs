{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Control.Monad (unless)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Parser
import System.IO (hFlush, stdout, isEOF)
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)
import Eval (eval, showVal, runEval, printError, Env (..))
import qualified Data.Map as Map
import Data.IORef (newIORef)
import Prim (primEnv)

repl :: Env -> IO ()
repl env = do
  putStr "> "
  hFlush stdout
  eof <- isEOF
  unless eof $ do
    line <- TIO.getLine
    unless (T.null line) $ do
      case parse single "<repl>" line of
        Left err -> putStrLn (errorBundlePretty err)
        Right ast -> do
          result <- runEval env (eval ast)
          case result of
            Left err -> printError err
            Right v -> TIO.putStrLn $ showVal v
    repl env

main :: IO ()
main = do
  prims <- primEnv
  bindings <- newIORef $ Map.fromList []
  repl $ Env { parent = Just prims, bindings }
