{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Monad.Writer
import           Data.Aeson
import qualified Data.ByteString.Lazy as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import           JL.Functions
import           JL.Inferer
import           JL.Interpreter
import           JL.Parser
import           JL.Printer
import           JL.Serializer
import           JL.Types
import           Options.Applicative.Simple

main :: IO ()
main = do
  do ((inp, aslines, browse), ()) <-
       simpleOptions
         "0.0.0"
         "jl - JSON Lambda calculus"
         "Command-line language for querying and outputting JSON."
         ((,,) <$> strArgument (metavar "CODE") <*>
          flag
            False
            True
            (short 'l' <> long "lines" <>
             help
               "Output each element of arrays on a newline suitable for UNIX piping") <*>
          flag
            False
            True
            (long "browse" <> help "Prints out all available functions"))
         empty
     if browse
       then mapM_
              (\((Variable name), (_, ty)) -> T.putStrLn (name <> " :: " <> prettyType ty))
              (M.toList bindings)
       else case parseText "" (T.pack inp) of
              Left err -> error (show err)
              Right expr0 -> do
                L.interact
                  (\js ->
                     case decode js of
                       Nothing -> error "invalid JSON"
                       Just j ->
                         let expr =
                               ApplicationExpression expr0 (valueToExpression j)
                         in case infer context expr (map TypeVariable [1 ..]) of
                              !_ ->
                                case eval
                                       (foldl
                                          (\e (v, (f, _)) -> subst v f e)
                                          (desugar expr)
                                          (M.toList bindings)) of
                                  v ->
                                    if aslines
                                      then L.intercalate
                                             "\n"
                                             (map
                                                encode
                                                (V.toList
                                                   (asArray (coreToValue v)))) <>
                                           "\n"
                                      else encode (coreToValue v) <> "\n"
                                    where asArray =
                                            \case
                                              Array xs -> xs
                                              x -> V.singleton x)
