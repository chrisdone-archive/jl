{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

-- |

module JL.Printer where

import           Control.Monad.Writer
import           Data.Aeson
import qualified Data.ByteString.Lazy as L
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.HashMap.Strict as HM
import           JL.Types

-- | Pretty printing for type.
prettyType :: Type -> Text
prettyType = render
  where
    render = go
      where
        go t =
          case t of
            ValueType -> "Value"
            VariableType (TypeVariable n) -> nameStream !! n
            FunctionType a b ->
              (case a of
                 FunctionType {} -> "(" <> go a <> ")"
                 _ -> go a) <>
              " → " <>
              go b
    nameStream = go 'a' (0 :: Integer)
      where
        go c n =
          (T.pack
             (c :
              if n == 0
                then ""
                else show n)) :
          (go
             (toEnum (fromEnum 'a' + (mod (fromEnum c - fromEnum 'a' + 1) 26)))
             (if c == 'z'
                then n + 1
                else n))

-- | Pretty printing for expression.
prettyExp :: Expression -> Text
prettyExp = go
  where
    go t =
      case t of
        ValueExpression v -> T.decodeUtf8 (L.toStrict (encode v))
        VariableExpression (Variable name) -> name
        LambdaExpression (Variable n) _ty e ->
          "(\\" <> n <> " -> " <> go e <> ")"
        ApplicationExpression f x -> "(" <> go f <> " (" <> go x <> "))"
        InfixExpression l o r ->
          prettyExp l <> " " <> prettyVariable o <> " " <> prettyExp r
        IfExpression a b c ->
          "if " <> prettyExp a <> " then " <> prettyExp b <> " else " <>
          prettyExp c
        RecordExpression hm ->
          "{" <>
          T.intercalate
            ", "
            (map (\(k, v) -> k <> ": " <> prettyExp v) (HM.toList hm)) <>
          "}"
        EvalExpression _ -> "<builtin>"

prettyVariable :: Variable -> Text
prettyVariable (Variable t) = t
