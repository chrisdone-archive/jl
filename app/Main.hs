{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Monad.Writer
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Aeson.Parser as Aeson
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Conduit ( ($=), ($$))
import           Data.Conduit.Attoparsec
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
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
import           System.IO

main :: IO ()
main = do
  do hSetBuffering stdout LineBuffering
     hSetBuffering stdin LineBuffering
     ((inp, file, aslines, browse, markdown, pretty), ()) <-
       simpleOptions
         "0.0.0"
         "jl - JSON Lambda calculus"
         "Command-line language for querying and outputting JSON."
         ((,,,,,) <$>
          strArgument
            (metavar "CODE" <>
             help "JL code; supports completion of function names" <>
             completeWith (map (\(Variable v) -> T.unpack v) (M.keys context))) <*>
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
             help "Prints out all available functions, in markdown format") <*>
          flag
            False
            True
            (short 'p' <> long "pretty" <>
             help "Outputs JSON in a human-friendly format"))
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
                    case Aeson.decode bytes of
                      Nothing -> hPutStr stderr "invalid input JSON"
                      Just j -> handleJson pretty expr0 aslines j
                  Nothing -> process pretty expr0 aslines
  where
    process pretty expr0 aslines =
      CB.sourceHandle stdin $= CB.lines $= conduitParserEither Aeson.value $=
      CL.mapM_
        (either
           (hPutStrLn stderr . errorMessage)
           (handleJson pretty expr0 aslines . snd)) $$
      CL.sinkNull

-- | Handle a JSON input, printing out one to many JSON values.
handleJson :: Bool -> Expression -> Bool -> Aeson.Value -> IO ()
handleJson pretty expr0 aslines j =
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
               then mapM_
                      L8.putStrLn
                      (map Aeson.encode (V.toList (asArray (coreToValue v))))
               else L8.putStrLn (encode' (coreToValue v))
             where asArray =
                     \case
                       Aeson.Array xs -> xs
                       x -> V.singleton x
                   encode' =
                     if pretty
                       then Aeson.encodePretty
                       else Aeson.encode
