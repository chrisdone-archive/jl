{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |

module JL.Parser where

import           Control.Monad.Catch
import           Data.Functor
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import           JL.Tokenizer
import           JL.Types
import           Text.Parsec hiding (satisfy, anyToken)

parseText :: MonadThrow m => SourceName -> Text -> m Expression
parseText fp inp =
  case parse tokensTokenizer fp (inp) of
    Left e -> throwM (TokenizerError e)
    Right tokens' ->
      case runParser (expressionParser <* endOfTokens) 0 fp tokens' of
        Left e -> throwM (ParserError e)
        Right ast -> pure ast

expressionParser :: TokenParser Expression
expressionParser = pipes
  where
    pipes = do
      ps <- sepBy1 dollars (equalToken Bar)
      case ps of
        [p] -> pure p
        [] -> unexpected "empty expression"
        (p:ps') ->
          pure
            (foldl
               (\x y ->
                  ApplicationExpression
                    (ApplicationExpression
                       (VariableExpression (Variable "compose"))
                       x)
                    y)
               p
               ps')
    dollars = do
      ps <- sepBy1 dollarable (equalToken Dollar)
      case ps of
        [p] -> pure p
        (p:ps') -> pure (foldl ApplicationExpression p ps')
        [] -> unexpected "empty expression"
      where
        dollarable =
          array <|> record <|> lambda <|> ifParser <|> infix' <|> app <|> atomic
    array = do
      void (equalToken OpenBracket) <?> ("open bracket " <> curlyQuotes "[")
      es <- sepBy expressionParser (void (equalToken Comma))
      void (equalToken CloseBracket) <?> ("closing bracket " <> curlyQuotes "]")
      pure (ArrayExpression (V.fromList es))
    record = do
      _ <- equalToken OpenBrace
      pairs' <- sepBy pair (equalToken Comma <?> curlyQuotes ",")
      _ <- equalToken CloseBrace <?> ("closing brace " <> curlyQuotes "}")
      pure (RecordExpression (HM.fromList pairs'))
      where
        pair = do
          var <-
            fmap
              fst
              (consumeToken
                 (\case
                    VariableToken i -> Just i
                    _ -> Nothing)) <|>
            fmap
              fst
              (consumeToken
                 (\case
                    StringToken c -> Just c
                    _ -> Nothing))
          _ <- equalToken Colon
          e <- expressionParser
          pure (var, e)
    app = do
      left <- funcOp <?> "function expression"
      right <- many unambiguous <?> "function arguments"
      case right of
        [] -> pure left
        _ -> pure (foldl (ApplicationExpression) left right)
    infix' =
      (do left <- (app <|> unambiguous) <?> "left-hand side of operator"
          tok <- fmap Just (operator <?> "infix operator") <|> pure Nothing
          case tok of
            Just (Operator t, _) -> do
              right <-
                (app <|> unambiguous) <?>
                ("right-hand side of " ++
                 curlyQuotes (T.unpack t) ++ " operator")
              badop <- fmap Just (lookAhead operator) <|> pure Nothing
              let infixexp = InfixExpression left (Variable t) right
              maybe
                (return ())
                (\op ->
                   unexpected
                     (concat
                        [ tokenString op ++
                          ". When more than one operator is used\n"
                        , "in the same expression, use parentheses."
                        ]))
                badop
              pure infixexp
            _ -> pure left) <?>
      "infix expression (e.g. x * y)"
      where
        operator =
          satisfyToken
            (\case
               Operator {} -> True
               _ -> False)
    funcOp = do
      let collectsubscripts ks a = do
            bracket' <-
              fmap (const True) (equalToken OpenBracket) <|> pure False
            if bracket'
              then do
                k <- expressionParser
                _ <- equalToken CloseBracket
                collectsubscripts (ks . (ExpressionSubscript k :)) a
              else do
                dot <- fmap (const True) (equalToken Period) <|> pure False
                if dot
                  then do
                    k <-
                      fmap
                        fst
                        (consumeToken
                           (\case
                              VariableToken i -> Just i
                              Integer i -> Just (T.pack (show i))
                              _ -> Nothing))
                    collectsubscripts (ks . (PropertySubscript k :)) a
                  else pure (ks [], a)
      a <- varParser <|> parensExpr
      (subscripts, b) <- collectsubscripts id a
      if null subscripts
         then case b of
                VariableExpression (Variable "_") -> unexpected "wildcard without subscript"
                _ -> pure a
         else pure (SubscriptExpression
                      (case b of
                         VariableExpression (Variable "_") -> WildcardSubscripted
                         _ -> ExpressionSubscripted b)
                      subscripts)
    unambiguous = funcOp <|> record <|> atomic
    parensExpr = parens expressionParser

parens :: TokenParser a -> TokenParser a
parens p = go <?> "parens e.g. (x)"
  where
    go = do
      _ <- equalToken OpenParen
      e <- p <?> "expression inside parentheses e.g. (foo)"
      _ <- equalToken CloseParen <?> "closing parenthesis ‘)’"
      pure e

varParser :: TokenParser Expression
varParser = go <?> "variable (e.g. ‘foo’, ‘id’, etc.)"
  where
    go = do
      (v, _) <-
        consumeToken
          (\case
             VariableToken i -> Just i
             _ -> Nothing)
      pure
        (VariableExpression (Variable v))

ifParser :: TokenParser Expression
ifParser = go <?> "if expression (e.g. ‘if p then x else y’)"
  where
    go = do
      _ <- equalToken If
      p <- expressionParser <?> "condition expresion of if-expression"
      _ <- equalToken Then <?> "‘then’ keyword for if-expression"
      e1 <- expressionParser <?> "‘then’ clause of if-expression"
      _ <- equalToken Else <?> "‘else’ keyword for if-expression"
      e2 <- expressionParser <?> "‘else’ clause of if-expression"
      pure (IfExpression p e1 e2)

atomic :: TokenParser Expression
atomic =
  nullParser <|> boolParser <|> varParser <|> stringParser <|> integerParser <|>
  decimalParser
  where

    integerParser = go <?> "integer (e.g. 42, 123)"
      where
        go = do
          (c, _) <-
            consumeToken
              (\case
                 Integer c -> Just c
                 _ -> Nothing)
          pure (ConstantExpression (NumberConstant (fromIntegral c)))
    decimalParser = go <?> "decimal (e.g. 42, 123)"
      where
        go = do
          (c, _) <-
            consumeToken
              (\case
                 Decimal c -> Just c
                 _ -> Nothing)
          pure (ConstantExpression (NumberConstant (realToFrac c)))
    boolParser = go <?> "boolean (e.g. true, false)"
      where
        go = do
          (c, _) <-
            consumeToken
              (\case
                 TrueToken -> pure True
                 FalseToken -> pure False
                 _ -> Nothing)
          pure (ConstantExpression (BoolConstant c))
    nullParser = go <?> "null"
      where
        go = do
          ((), _) <-
            consumeToken
              (\case
                 NullToken -> pure ()
                 _ -> Nothing)
          pure (ConstantExpression NullConstant)

stringParser :: TokenParser Expression
stringParser = go <?> "string (e.g. \"a\")"
  where
    go = do
      (c, _) <-
        consumeToken
          (\case
             StringToken c -> Just c
             _ -> Nothing)
      pure (ConstantExpression (StringConstant c))

lambda :: TokenParser (Expression)
lambda = do
  _ <- equalToken Backslash <?> "lambda expression (e.g. \\x -> x)"
  args <- many1 funcParam <?> "lambda parameters"
  _ <- equalToken RightArrow
  e <- expressionParser
  pure (foldl (\e' arg -> LambdaExpression arg e') e (reverse args))

funcParam :: TokenParser Variable
funcParam = go <?> "function parameter (e.g. ‘x’, ‘limit’, etc.)"
  where
    go = do
      (v, _) <-
        consumeToken
          (\case
             VariableToken i -> Just i
             _ -> Nothing)
      pure (Variable v)
