{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

-- |

module JL.Interpreter where

import           Control.Monad.Writer
import           Data.Aeson
import qualified Data.Text as T
import           JL.Printer
import           JL.Types

-- | Eval expression.
eval :: Expression -> Expression
eval (ApplicationExpression op arg) =
  case eval op of
    LambdaExpression param _ expr ->
      case eval arg of
        a@VariableExpression {} ->
          error (T.unpack (prettyExp a <> " is not in scope!"))
        a -> eval (subst param a expr)
    EvalExpression f -> f (eval arg)
    _ -> error (T.unpack (prettyExp op <> " is not a function!"))
eval (IfExpression c a b) =
  case eval c of
    ValueExpression (Bool True) -> eval a
    ValueExpression (Bool False) -> eval b
    _ -> error ("type error for if condition, should be bool")
eval (InfixExpression a f b) =
  eval
    (ApplicationExpression (ApplicationExpression (VariableExpression f) a) b)
eval (RecordExpression hms) =
  RecordExpression (fmap eval hms)
eval e = e

-- | Substitute name in function body.
subst :: Variable -> Expression -> Expression -> Expression
subst name val e@(VariableExpression name')
  | name == name' = val
  | otherwise = e
subst name val (LambdaExpression name' ty e)
  | name /= name' = LambdaExpression name' ty (subst name val e)
subst name val (ApplicationExpression f a) =
  ApplicationExpression (subst name val f) (subst name val a)
subst name val (IfExpression f a b) =
  IfExpression (subst name val f) (subst name val a) (subst name val b)
subst name val (InfixExpression a f b) =
  ApplicationExpression
    (ApplicationExpression (subst name val (VariableExpression f)) (subst name val a))
    (subst name val b)
subst name val (RecordExpression hm) = RecordExpression (fmap (subst name val) hm)
subst _ _ e = e
