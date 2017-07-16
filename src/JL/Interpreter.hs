{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

-- |

module JL.Interpreter where

import           Control.Monad.Writer
import qualified Data.Text as T
import           JL.Printer
import           JL.Types

-- | Eval core.
eval :: Core -> Core
eval (ApplicationCore op arg) =
  case eval op of
    LambdaCore param expr ->
      case eval arg of
        a@VariableCore {} ->
          error (T.unpack (prettyCore a <> " is not in scope!"))
        a -> eval (subst param a expr)
    EvalCore f -> f (eval arg)
    _ -> error (T.unpack (prettyCore op <> " is not a function!"))
eval (IfCore c a b) =
  case eval c of
    ConstantCore (BoolConstant True) -> eval a
    ConstantCore (BoolConstant False) -> eval b
    _ -> error ("type error for if condition, should be bool")
eval (RecordCore hms) =
  RecordCore (fmap eval hms)
eval e = e

-- | Substitute name in function body.
subst :: Variable -> Core -> Core -> Core
subst name val e@(VariableCore name')
  | name == name' = val
  | otherwise = e
subst name val (LambdaCore name' e)
  | name /= name' = LambdaCore name' (subst name val e)
subst name val (ApplicationCore f a) =
  ApplicationCore (subst name val f) (subst name val a)
subst name val (IfCore f a b) =
  IfCore (subst name val f) (subst name val a) (subst name val b)
subst name val (RecordCore hm) = RecordCore (fmap (subst name val) hm)
subst _ _ e = e

-- | Remove syntactic sugar and convert into executable form.
desugar :: Expression -> Core
desugar =
  \case
    (InfixExpression a f b) ->
      ApplicationCore (ApplicationCore (VariableCore f) (desugar a)) (desugar b)
    VariableExpression v -> VariableCore v
    LambdaExpression v _ e -> LambdaCore v (desugar e)
    ApplicationExpression f a -> ApplicationCore (desugar f) (desugar a)
    IfExpression a b c -> IfCore (desugar a) (desugar b) (desugar c)
    RecordExpression pars -> RecordCore (fmap desugar pars)
    ArrayExpression as -> ArrayCore (fmap desugar as)
    ConstantExpression c -> ConstantCore c
    SubscriptExpression subscripted subscripts ->
      let index =
            \c k ->
              ApplicationCore
                (ApplicationCore
                   (VariableCore (Variable "get"))
                   (case k of
                      ExpressionSubscript e -> desugar e
                      PropertySubscript t -> ConstantCore (StringConstant t)))
                c
      in case subscripted of
           WildcardSubscripted ->
             LambdaCore
               (Variable "a'")
               (foldl index (VariableCore (Variable "a'")) subscripts)
           ExpressionSubscripted e -> foldl index (desugar e) subscripts
