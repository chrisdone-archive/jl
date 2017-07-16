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
            (prettyCore
               (eval (foldl (\e (v, (f, _)) -> subst v f e) (desugar expr) (M.toList bindings))))
          where expr = ApplicationExpression expr0 (valueToExpression j)

context :: Map Variable Type
context = fmap snd bindings

scope :: Map Variable Core
scope = fmap fst bindings

bindings :: Map Variable (Core, Type)
bindings = M.fromList (concat [arith, records, arrays, funcs])

funcs :: [(Variable, (Core, Type))]
funcs = [idf, compose]
  where
    idf =
      ( Variable "id"
      , (EvalCore (\x -> x), FunctionType ValueType ValueType))
    compose =
      ( Variable "compose"
      , ( LambdaCore
            (Variable "f")
            (LambdaCore
               (Variable "g")
               (LambdaCore
                  (Variable "x")
                  (ApplicationCore
                     (VariableCore (Variable "g"))
                     (ApplicationCore
                        (VariableCore (Variable "f"))
                        (VariableCore (Variable "x"))))))
        , FunctionType
            (FunctionType ValueType ValueType)
            (FunctionType
               (FunctionType ValueType ValueType)
               (FunctionType ValueType ValueType))))

arrays :: [(Variable, (Core, Type))]
arrays = [mapf, filterf, len, takef, dropf, empty, concatf, rev, zipw]
  where
    zipw =
      ( Variable "zipwith"
      , ( EvalCore
            (\f ->
               EvalCore
                 (\xs ->
                    EvalCore
                      (\ys ->
                         case (xs, ys) of
                           (ArrayCore xs', ArrayCore ys') ->
                             (ArrayCore
                                (V.zipWith
                                   (\x y ->
                                      eval (ApplicationCore (ApplicationCore f x) y))
                                   xs'
                                   ys'))
                           _ -> error "can only zip two arrays")))
        , FunctionType ValueType (FunctionType ValueType ValueType)))
    takef =
      ( Variable "take"
      , ( EvalCore
            (\n ->
               EvalCore
                 (\xs ->
                    case (n, xs) of
                      (ConstantCore (NumberConstant n'), ArrayCore xs') ->
                        (ArrayCore (V.take (round n') xs'))
                      _ -> error "can only take from arrays"))
        , FunctionType ValueType (FunctionType ValueType ValueType)))
    dropf =
      ( Variable "drop"
      , ( EvalCore
            (\n ->
               EvalCore
                 (\xs ->
                    case (n, xs) of
                      (ConstantCore (NumberConstant n'), ArrayCore xs') ->
                        (ArrayCore (V.drop (round n') xs'))
                      _ -> error "can only drop from arrays"))
        , FunctionType ValueType (FunctionType ValueType ValueType)))
    concatf =
      ( Variable "concat"
      , ( (EvalCore
             (\xs ->
                case xs of
                  (ArrayCore xs') ->
                    (ArrayCore (V.concat (map coreToArray (V.toList xs'))))
                  _ -> error "can only take length of arrays"))
        , FunctionType ValueType ValueType))
    rev =
      ( Variable "reverse"
      , ( (EvalCore
             (\xs ->
                case xs of
                  (ArrayCore xs') -> (ArrayCore (V.reverse xs'))
                  _ -> error "can only reverse an array or a string"))
        , FunctionType ValueType ValueType))
    len =
      ( Variable "length"
      , ( (EvalCore
             (\xs ->
                case xs of
                  (ArrayCore xs') ->
                    (ConstantCore (NumberConstant (fromIntegral (V.length xs'))))
                  _ -> error "can only take length of arrays"))
        , FunctionType ValueType ValueType))
    empty =
      ( Variable "empty"
      , ( (EvalCore
             (\xs ->
                case xs of
                  (ArrayCore xs') -> (ConstantCore (BoolConstant (V.null xs')))
                  _ -> error "can only check if arrays are empty"))
        , FunctionType ValueType ValueType))
    filterf =
      ( Variable "filter"
      , ( EvalCore
            (\f ->
               EvalCore
                 (\xs ->
                    case xs of
                      (ArrayCore xs') ->
                        (ArrayCore
                           (V.filter
                              (\x ->
                                 case eval (ApplicationCore f x) of
                                   ConstantCore (BoolConstant b) -> b
                                   _ -> True)
                              xs'))
                      _ -> error "can only filter over arrays"))
        , FunctionType
            (FunctionType ValueType ValueType)
            (FunctionType ValueType ValueType)))
    mapf =
      ( Variable "map"
      , ( EvalCore
            (\f ->
               EvalCore
                 (\xs ->
                    case xs of
                      (ArrayCore xs') ->
                        (ArrayCore
                           (fmap (\x -> (eval (ApplicationCore f (x)))) xs'))
                      _ -> error "can only map over arrays"))
        , FunctionType
            (FunctionType ValueType ValueType)
            (FunctionType ValueType ValueType)))

coreToArray :: Core -> V.Vector Core
coreToArray =
  \case
    ArrayCore xs -> xs
    x -> error ("expected array but found single value: " <> T.unpack (prettyCore x))

arith :: [(Variable, (Core, Type))]
arith =
  [ binop "*" (*)
  , binop "+" (+)
  , binop "-" (-)
  , binop "/" (/)
  , boolop "/=" (/=)
  , boolop "=" (==)
  , boolnumop ">" (>)
  , boolnumop "<" (<)
  , boolnumop ">=" (>=)
  , boolnumop "<=" (<=)
  ]
  where
    binop name f =
      ( Variable name
      , ( EvalCore
            (\x ->
               EvalCore
                 (\y ->
                    case (x, y) of
                      (ConstantCore (NumberConstant a), ConstantCore (NumberConstant b)) ->
                        ConstantCore (NumberConstant (f a b))
                      _ -> error ("type error for arguments to " <> show name)))
        , FunctionType ValueType (FunctionType ValueType ValueType)))
    boolop name f =
      ( Variable name
      , ( EvalCore
            (\x ->
               EvalCore
                 (\y ->
                    ConstantCore
                      (BoolConstant (f (coreToValue x) (coreToValue y)))))
        , FunctionType ValueType (FunctionType ValueType ValueType)))
    boolnumop name f =
      ( Variable name
      , ( EvalCore
            (\x ->
               EvalCore
                 (\y ->
                    case (x, y) of
                      (ConstantCore (NumberConstant a), ConstantCore (NumberConstant b)) ->
                        ConstantCore (BoolConstant (f a b))
                      _ -> error ("type error for arguments to " <> show name)))
        , FunctionType ValueType (FunctionType ValueType ValueType)))

records :: [(Variable, (Core, Type))]
records = [getf
  , setf,modifyf]
  where modifyf =
          ( Variable "modify"
          , ( EvalCore
                (\key ->
                   EvalCore
                     (\f ->
                        EvalCore
                          (\obj ->
                             case (key, obj) of
                               (ConstantCore (StringConstant k), (RecordCore o)) ->
                                 (RecordCore
                                      (HM.adjust
                                         (\v ->
                                            eval
                                              (ApplicationCore
                                                 f
                                                 v))
                                         k
                                         o))
                               _ -> error "type error for args to modify")))
            , FunctionType
                ValueType
                (FunctionType
                   (FunctionType ValueType ValueType)
                   (FunctionType ValueType ValueType))))
        getf =
          ( Variable "get"
          , ( EvalCore
                (\key ->
                   EvalCore
                     (\obj ->
                        case (key, obj) of
                          (ConstantCore (StringConstant k), RecordCore o) ->
                              (case HM.lookup k o of
                                 Nothing -> error ("missing key " <> show k)
                                 Just v -> v)
                          (ConstantCore (NumberConstant i), (ArrayCore v)) ->

                              (case v V.!? (round i) of
                                 Nothing -> error ("missing array index " <> show i)
                                 Just v' -> v')
                          _ -> error "type error for args"))
            , FunctionType ValueType (FunctionType ValueType ValueType)))
        setf =
          ( Variable "set"
          , ( EvalCore
                (\key ->
                   EvalCore
                     (\val ->
                        EvalCore
                          (\obj ->
                             case (key, val, obj) of
                               (ConstantCore (StringConstant k), v, RecordCore o) ->
                                 (RecordCore (HM.insert k v o))
                               _ -> error "type error in arguments to: set")))
            , FunctionType
                ValueType
                (FunctionType ValueType (FunctionType ValueType ValueType))))

coreToValue :: Core -> Value
coreToValue =
  \case
    ConstantCore v -> constantToValue v
    RecordCore hm -> Object (fmap coreToValue hm)
    ArrayCore a -> Array (fmap coreToValue a)
    e -> error ("code generated invalid JSON: " <> T.unpack (prettyCore e))

constantToValue :: Constant -> Value
constantToValue =
  \case
    NullConstant -> Null
    BoolConstant b -> Bool b
    StringConstant s -> String s
    NumberConstant n -> Number n

valueToCore :: Value -> Core
valueToCore =
  \case
    Object os -> RecordCore (fmap valueToCore os)
    Array xs -> ArrayCore (fmap valueToCore xs)
    Number n -> ConstantCore (NumberConstant n)
    Bool n -> ConstantCore (BoolConstant n)
    Null -> ConstantCore NullConstant
    String t -> ConstantCore (StringConstant t)

valueToExpression :: Value -> Expression
valueToExpression =
  \case
    Object os -> RecordExpression (fmap valueToExpression os)
    Array xs -> ArrayExpression (fmap valueToExpression xs)
    Number n -> ConstantExpression (NumberConstant n)
    Bool n -> ConstantExpression (BoolConstant n)
    Null -> ConstantExpression NullConstant
    String t -> ConstantExpression (StringConstant t)
