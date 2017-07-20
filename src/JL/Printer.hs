{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

-- |

module JL.Printer where

import           Control.Monad.Writer
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import           JL.Types

-- | Pretty printing for type.
prettyType :: Type -> Text
prettyType = render
  where
    render = go
      where
        go t =
          case t of
            JSONType -> "JSON"
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
        VariableExpression (Variable name) -> name
        LambdaExpression (Variable n) e ->
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
        SubscriptExpression o ks ->
          (case o of
             WildcardSubscripted -> "_"
             ExpressionSubscripted e -> go e) <>
          mconcat (map prettySubcript ks)
        ArrayExpression as ->
          "[" <> T.intercalate ", " (map prettyExp (V.toList as)) <> "]"
        ConstantExpression c -> prettyConstant c

-- | Pretty printing for core.
prettyCore :: Core -> Text
prettyCore = go
  where
    go t =
      case t of
        VariableCore (Variable name) -> name
        LambdaCore (Variable n) e -> "(\\" <> n <> " -> " <> go e <> ")"
        ApplicationCore f x -> "(" <> go f <> " (" <> go x <> "))"
        IfCore a b c ->
          "if " <> prettyCore a <> " then " <> prettyCore b <> " else " <>
          prettyCore c
        RecordCore hm ->
          "{" <>
          T.intercalate
            ", "
            (map (\(k, v) -> k <> ": " <> prettyCore v) (HM.toList hm)) <>
          "}"
        EvalCore _ -> "<internal>"
        ArrayCore as ->
          "[" <> T.intercalate ", " (map prettyCore (V.toList as)) <> "]"
        ConstantCore c -> prettyConstant c

prettyConstant :: Constant -> Text
prettyConstant =
  \case
    NumberConstant s -> T.pack (show s)
    StringConstant t -> T.pack (show t)
    BoolConstant b ->
      if b
        then "true"
        else "false"
    NullConstant -> "null"

prettySubcript :: Subscript -> Text
prettySubcript =
  \case
    ExpressionSubscript e -> "[" <> prettyExp e <> "]"
    PropertySubscript p -> "." <> p

prettyVariable :: Variable -> Text
prettyVariable (Variable t) = t
