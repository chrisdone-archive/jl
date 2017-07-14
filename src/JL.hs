{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Main library entry point.

module JL where

import           Control.Monad.Writer
import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import qualified Data.HashMap.Strict as HM
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           JL.Inferer
import           JL.Interpreter
import           JL.Parser
import           JL.Printer
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
            (prettyExp
               (eval (foldl (\e (v, (f, _)) -> subst v f e) expr (M.toList bindings))))
          where expr = ApplicationExpression expr0 (ValueExpression j)

context :: Map Variable Type
context = fmap snd bindings

scope :: Map Variable Expression
scope = fmap fst bindings

bindings :: Map Variable (Expression, Type)
bindings = M.fromList (concat [arith, records, arrays, funcs])

funcs :: [(Variable, (Expression, Type))]
funcs = [idf, compose]
  where
    idf =
      ( Variable "id"
      , (EvalExpression (\x -> x), FunctionType ValueType ValueType))
    compose =
      ( Variable "compose"
      , ( LambdaExpression
            (Variable "f")(VariableType (TypeVariable 0))
            (LambdaExpression
               (Variable "g") (VariableType (TypeVariable 2))
               (LambdaExpression
                  (Variable "x") (VariableType (TypeVariable 1))
                  (ApplicationExpression
                     (VariableExpression (Variable "g"))
                     (ApplicationExpression
                        (VariableExpression (Variable "f"))
                        (VariableExpression (Variable "x"))))))
        , FunctionType
            (FunctionType ValueType ValueType)
            (FunctionType
               (FunctionType ValueType ValueType)
               (FunctionType ValueType ValueType))))

arrays :: [(Variable, (Expression, Type))]
arrays = [mapf, modifyf]
  where
    modifyf =
      ( Variable "modify"
      , ( EvalExpression
            (\key ->
               EvalExpression
                 (\f ->
                    EvalExpression
                      (\obj ->
                         case (key, obj) of
                           (ValueExpression (String k), ValueExpression (Object o)) ->
                             ValueExpression
                               (Object
                                  (HM.adjust
                                     (\v ->
                                        case eval
                                               (ApplicationExpression
                                                  f
                                                  (ValueExpression v)) of
                                          ValueExpression vv -> vv
                                          _ ->
                                            error
                                              "modify did not return a regular JSON value")
                                     k
                                     o))
                           _ -> error "type error for args")))
        , FunctionType
            ValueType
            (FunctionType (FunctionType ValueType ValueType) (FunctionType ValueType ValueType))))
    mapf =
      ( Variable "map"
      , ( EvalExpression
            (\f ->
               EvalExpression
                 (\xs ->
                    case xs of
                      ValueExpression (Array xs') ->
                        ValueExpression
                          (Array
                             (fmap
                                (\x ->
                                   toValue (eval
                                              (ApplicationExpression
                                                 f
                                                 (ValueExpression x))))
                                xs'))
                      _ -> error "can only map over arrays"))
        , FunctionType
            (FunctionType ValueType ValueType)
            (FunctionType ValueType ValueType)))

arith :: [(Variable, (Expression, Type))]
arith = [binop "*" (*)
  , binop "+" (+)
  , binop "-" (-)
  , binop "/" (/)]
  where binop name f =
          ( Variable name
          , ( EvalExpression
                (\x ->
                   EvalExpression
                     (\y ->
                        case (x, y) of
                          (ValueExpression (Number a), ValueExpression (Number b)) ->
                            ValueExpression (Number (f a b))
                          _ -> error "type error for arguments"))
            , FunctionType ValueType (FunctionType ValueType ValueType)))

records :: [(Variable, (Expression, Type))]
records = [getf
  , setf]
  where getf =
          ( Variable "get"
          , ( EvalExpression
                (\key ->
                   EvalExpression
                     (\obj ->
                        case (key, toValue obj) of
                          (ValueExpression (String k), (Object o)) ->
                            ValueExpression
                              (case HM.lookup k o of
                                 Nothing -> error ("missing key " <> show k)
                                 Just v -> v)
                          (ValueExpression (Number i), (Array v)) ->
                            ValueExpression
                              (case v V.!? (round i) of
                                 Nothing -> error ("missing array index " <> show i)
                                 Just v -> v)
                          _ -> error "type error for args"))
            , FunctionType ValueType (FunctionType ValueType ValueType)))
        setf =
          ( Variable "set"
          , ( EvalExpression
                (\key ->
                   EvalExpression
                     (\val ->
                        EvalExpression
                          (\obj ->
                             case (key, val, obj) of
                               (ValueExpression (String k), ValueExpression v, ValueExpression (Object o)) ->
                                 ValueExpression (Object (HM.insert k v o))
                               _ -> error "type error for args")))
            , FunctionType
                ValueType
                (FunctionType ValueType (FunctionType ValueType ValueType))))


toValue :: Expression -> Value
toValue =
  \case
    ValueExpression v -> v
    RecordExpression hm -> Object (fmap toValue hm)
    e -> error ("code generated invalid JSON: " <> T.unpack (prettyExp e))

-- data Value = Object !Object
--            | Array !Array
--            | String !Text
--            | Number !Scientific
--            | Bool !Bool
--            | Null
--              deriving (Eq, Read, Show, Typeable, Data, Generic)
