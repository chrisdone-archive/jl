{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Main library entry point.

module JL where

import           Control.Monad.Writer
import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text.IO as T
import           JL.Functions
import           JL.Inferer
import           JL.Interpreter
import           JL.Parser
import           JL.Printer
import           JL.Serializer
import           JL.Types

repl :: Text -> ByteString -> IO ()
repl inp js =
  case decode js of
    Nothing -> error "Invalid JSON"
    Just j ->
      case parseText "" inp of
        Left err -> error (show err)
        Right expr0 -> do
          T.putStrLn
            (prettyExp expr <> " : " <>
             prettyType (infer context expr (map TypeVariable [1 ..])))
          T.putStrLn
            (prettyCore
               (eval (foldl (\e (v, (f, _)) -> subst v f e) (desugar expr) (M.toList bindings))))
          where expr = ApplicationExpression expr0 (valueToExpression j)
