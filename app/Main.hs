{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Monad.Writer
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8
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
  do ((inp, file, aslines, browse, markdown), ()) <-
       simpleOptions
         "0.0.0"
         "jl - JSON Lambda calculus"
         "Command-line language for querying and outputting JSON."
         ((,,,,) <$> strArgument (metavar "CODE") <*>
          optional (strArgument (metavar "FILE")) <*>
          flag
            False
            True
            (short 'l' <> long "lines" <>
             help
               "Output each element of arrays on a newline suitable for UNIX piping") <*>
          flag
            False
            True
            (long "browse" <> help "Prints out all available functions") <*>
          flag
            False
            True
            (long "browse-markdown" <>
             help "Prints out all available functions, in markdown format"))
         empty
     let block xs =
           if markdown
             then "```haskell\n" <> xs <> "\n```"
             else xs
     if browse || markdown
       then mapM_
              (\(groupname, defs) ->
                 T.putStrLn
                   (((if markdown
                        then "## "
                        else "") <>
                     groupname) <>
                    "\n\n" <>
                    T.unlines
                      (map
                         (\def ->
                            (let Variable v = definitionName def
                             in block
                                  (v <> " :: " <>
                                   prettyType (definitionType def)) <>
                                "\n\n" <>
                                (if not markdown
                                   then "  "
                                   else "") <>
                                definitionDoc def <>
                                "\n"))
                         defs)))
              functions
       else case parseText "" (T.pack inp) of
              Left err -> error (show err)
              Right expr0 -> do
                case file of
                  Just fp -> do
                    bytes <- L.readFile fp
                    L8.putStr (process expr0 aslines bytes)
                  Nothing -> L.interact (process expr0 aslines)
  where
    process expr0 aslines =
      \js ->
        case decode js of
          Nothing -> error "invalid JSON"
          Just j ->
            let expr = ApplicationExpression expr0 (valueToExpression j)
            in case infer context expr (map TypeVariable [1 ..]) of
                 !_ ->
                   case eval
                          (foldl
                             (\e (v, f) -> subst v f e)
                             (desugar expr)
                             (M.toList scope)) of
                     v ->
                       if aslines
                         then L.intercalate
                                "\n"
                                (map encode (V.toList (asArray (coreToValue v)))) <>
                              "\n"
                         else encode (coreToValue v) <> "\n"
                       where asArray =
                               \case
                                 Array xs -> xs
                                 x -> V.singleton x
