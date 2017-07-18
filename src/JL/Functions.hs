{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |

module JL.Functions (context, scope, functions) where

import           Control.Arrow
import           Control.Monad.Writer
import           Data.Aeson (Value)
import qualified Data.HashMap.Strict as HM
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Scientific
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import           JL.Interpreter
import           JL.Printer
import           JL.Serializer
import           JL.Types

--------------------------------------------------------------------------------
-- Lists

-- | The typing context.
context :: Map Variable Type
context = M.fromList (map (definitionName &&& definitionType) (concatMap snd functions))

-- | Bindings available in scope.
scope :: Map Variable Core
scope = M.fromList (map (definitionName &&&definitionCore) (concatMap snd functions))

-- | All functions.
functions :: [(Text, [Definition])]
functions =
  [ ( "Arithmetic operators"
    , [ arithmeticOperator "*" (*)
      , arithmeticOperator "+" (+)
      , arithmeticOperator "-" (-)
      , arithmeticOperator "/" (/)
      ])
  , ( "Predicate operators"
    , [predicateOperator "/=" (/=), predicateOperator "=" (==)])
  , ( "Numeric predicate operators"
    , [ numericPredicateOperator ">" (>)
      , numericPredicateOperator "<" (<)
      , numericPredicateOperator ">=" (>=)
      , numericPredicateOperator "<=" (<=)
      ])
  , ("Function combinators", [idf, compose, flipf])
  , ("Record access", [getf, setf, modifyf])
  , ( "Sequences"
    , [ mapf
      , filterf
      , takeWhilef
      , empty
      , len
      , rev
      , dropf
      , elemf
      , concatf
      , zipw
      , takef
      , foldf
      , dropWhilef
      ])
  ]

--------------------------------------------------------------------------------
-- Functions

modifyf :: Definition
modifyf =
  Definition
  { definitionDoc = "Modify the object at k with function f"
  , definitionName = Variable "modify"
  , definitionCore =
      (EvalCore
         (\key ->
            EvalCore
              (\f ->
                 EvalCore
                   (\obj ->
                      case (key, obj) of
                        (ConstantCore (StringConstant k), (RecordCore o)) ->
                          (RecordCore
                             (HM.adjust (\v -> eval (ApplicationCore f v)) k o))
                        _ -> error "type error for args to modify"))))
  , definitionType =
      FunctionType
        ValueType
        (FunctionType
           (FunctionType ValueType ValueType)
           (FunctionType ValueType ValueType))
  }

getf :: Definition
getf =
  Definition
  { definitionDoc = "Get the value at k from the object"
  , definitionName = Variable "get"
  , definitionCore =
      (EvalCore
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
                   _ -> error "type error for args")))
  , definitionType = FunctionType ValueType (FunctionType ValueType ValueType)
  }

setf :: Definition
setf =
  Definition
  { definitionDoc = "Set the value k to v in object"
  , definitionName = Variable "set"
  , definitionCore =
      (EvalCore
         (\key ->
            EvalCore
              (\val ->
                 EvalCore
                   (\obj ->
                      case (key, val, obj) of
                        (ConstantCore (StringConstant k), v, RecordCore o) ->
                          (RecordCore (HM.insert k v o))
                        _ -> error "type error in arguments to: set"))))
  , definitionType =
      FunctionType
        ValueType
        (FunctionType ValueType (FunctionType ValueType ValueType))
  }

idf :: Definition
idf =
  Definition
  { definitionDoc = "Identity function, returns its input unchanged"
  , definitionName = Variable "id"
  , definitionCore = (EvalCore (\x -> x))
  , definitionType = FunctionType ValueType ValueType
  }

flipf :: Definition
flipf =
  Definition
  { definitionDoc = "Flips the argument order of a function of two or more arguments"
  , definitionName = Variable "flip"
  , definitionCore =
      (EvalCore
         (\f ->
            EvalCore
              (\x ->
                 EvalCore (\y -> eval (ApplicationCore (ApplicationCore f y) x)))))
  , definitionType =
      FunctionType
        (FunctionType ValueType (FunctionType ValueType ValueType))
        (FunctionType ValueType (FunctionType ValueType ValueType))
  }

compose :: Definition
compose =
  Definition
  { definitionDoc = "Compose two functions"
  , definitionName = Variable "compose"
  , definitionCore =
      EvalCore
        (\f ->
           EvalCore
             (\g ->
                EvalCore (\x -> eval (ApplicationCore g (ApplicationCore f x)))))
  , definitionType =
      FunctionType
        (FunctionType ValueType ValueType)
        (FunctionType
           (FunctionType ValueType ValueType)
           (FunctionType ValueType ValueType))
  }

foldf :: Definition
foldf =
  Definition
  { definitionDoc = "Fold over a structure with a state."
  , definitionName = Variable "fold"
  , definitionCore =
      EvalCore
        (\cons ->
           EvalCore
             (\nil ->
                EvalCore
                  (\xs ->
                     case xs of
                       ArrayCore xs' ->
                         (V.foldl
                            (\acc x ->
                               eval
                                 (ApplicationCore (ApplicationCore cons acc) x))
                            nil
                            xs')
                       _ -> error "can only fold arrays")))
  , definitionType =
      (FunctionType
         ((FunctionType ValueType (FunctionType ValueType ValueType)))
         (FunctionType ValueType (FunctionType ValueType ValueType)))
  }

zipw :: Definition
zipw =
  Definition
  { definitionDoc = "Zip two lists calling with each element to f x y"
  , definitionName = Variable "zipWith"
  , definitionCore =
      EvalCore
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
  , definitionType =
      FunctionType
        (FunctionType ValueType (FunctionType ValueType ValueType))
        (FunctionType ValueType (FunctionType ValueType ValueType))
  }

elemf :: Definition
elemf =
  Definition
  { definitionDoc = "Is x an element of y?"
  , definitionName = Variable "elem"
  , definitionCore =
      EvalCore
        (\n ->
           EvalCore
             (\xs ->
                case xs of
                  (ArrayCore xs') ->
                    (ConstantCore
                       (BoolConstant
                          (V.elem (coreToValue n) (fmap coreToValue xs'))))
                  _ ->
                    error
                      ("can only check elements from arrays, args: " <>
                       T.unpack (prettyCore n) <>
                       " " <>
                       T.unpack (prettyCore xs))))
  , definitionType = FunctionType ValueType (FunctionType ValueType ValueType)
  }

takef :: Definition
takef =
  Definition
  { definitionDoc = "Take n items from sequence"
  , definitionName = Variable "take"
  , definitionCore =
      EvalCore
        (\n ->
           EvalCore
             (\xs ->
                case (n, xs) of
                  (ConstantCore (NumberConstant n'), ArrayCore xs') ->
                    (ArrayCore (V.take (round n') xs'))
                  _ -> error "can only take from arrays"))
  , definitionType = FunctionType ValueType (FunctionType ValueType ValueType)
  }

dropf :: Definition
dropf =
  Definition
  { definitionDoc = "Drop n items from the sequence"
  , definitionName = Variable "drop"
  , definitionCore =
      EvalCore
        (\n ->
           EvalCore
             (\xs ->
                case (n, xs) of
                  (ConstantCore (NumberConstant n'), ArrayCore xs') ->
                    (ArrayCore (V.drop (round n') xs'))
                  _ -> error "can only drop from arrays"))
  , definitionType = FunctionType ValueType (FunctionType ValueType ValueType)
  }

concatf :: Definition
concatf =
  Definition
  { definitionDoc = "Concatenate a list of sequences into one sequence"
  , definitionName = Variable "concat"
  , definitionCore =
      (EvalCore
         (\xs ->
            case xs of
              (ArrayCore xs') ->
                (ArrayCore (V.concat (map coreToArray (V.toList xs'))))
              _ -> error "can only concat arrays"))
  , definitionType = FunctionType ValueType ValueType
  }

rev :: Definition
rev =
  Definition
  { definitionDoc = "Reverse a sequence"
  , definitionName = Variable "reverse"
  , definitionCore =
      (EvalCore
         (\xs ->
            case xs of
              (ArrayCore xs') -> (ArrayCore (V.reverse xs'))
              _ -> error "can only reverse an array or a string"))
  , definitionType = FunctionType ValueType ValueType
  }

len :: Definition
len =
  Definition
  { definitionDoc = "Get the length of a sequence"
  , definitionName = Variable "length"
  , definitionCore =
      (EvalCore
         (\xs ->
            case xs of
              (ArrayCore xs') ->
                (ConstantCore (NumberConstant (fromIntegral (V.length xs'))))
              _ -> error "can only take length of arrays"))
  , definitionType = FunctionType ValueType ValueType
  }

empty :: Definition
empty =
  Definition
  { definitionDoc = "Is a sequence empty?"
  , definitionName = Variable "empty"
  , definitionCore =
      (EvalCore
         (\xs ->
            case xs of
              (ArrayCore xs') -> (ConstantCore (BoolConstant (V.null xs')))
              _ -> error "can only check if arrays are empty"))
  , definitionType = FunctionType ValueType ValueType
  }

dropWhilef :: Definition
dropWhilef =
  Definition
  { definitionDoc = "Drop elements from a sequence while a predicate is true"
  , definitionName = Variable "dropWhile"
  , definitionCore =
      EvalCore
        (\f ->
           EvalCore
             (\xs ->
                case xs of
                  (ArrayCore xs') ->
                    (ArrayCore
                       (V.dropWhile
                          (\x ->
                             case eval (ApplicationCore f x) of
                               ConstantCore (BoolConstant b) -> b
                               _ -> True)
                          xs'))
                  _ -> error "can only dropWhile over arrays"))
  , definitionType =
      FunctionType
        (FunctionType ValueType ValueType)
        (FunctionType ValueType ValueType)
  }

takeWhilef :: Definition
takeWhilef =
  Definition
  { definitionDoc = "Take elements from a sequence while given predicate is true"
  , definitionName = Variable "takeWhile"
  , definitionCore =
      (EvalCore
         (\f ->
            EvalCore
              (\xs ->
                 case xs of
                   (ArrayCore xs') ->
                     (ArrayCore
                        (V.takeWhile
                           (\x ->
                              case eval (ApplicationCore f x) of
                                ConstantCore (BoolConstant b) -> b
                                _ -> True)
                           xs'))
                   _ -> error "can only takeWhile over arrays")))
  , definitionType =
      FunctionType
        (FunctionType ValueType ValueType)
        (FunctionType ValueType ValueType)
  }

filterf :: Definition
filterf =
  Definition
  { definitionDoc = "Keep only items from the sequence for which p returns true"
  , definitionName = Variable "filter"
  , definitionCore =
      EvalCore
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
  , definitionType =
      FunctionType
        (FunctionType ValueType ValueType)
        (FunctionType ValueType ValueType)
  }

mapf :: Definition
mapf =
  Definition
  { definitionDoc = "Apply a function to every element in the sequence"
  , definitionName = Variable "map"
  , definitionCore =
      EvalCore
        (\f ->
           EvalCore
             (\xs ->
                case xs of
                  (ArrayCore xs') ->
                    (ArrayCore (fmap (\x -> (eval (ApplicationCore f (x)))) xs'))
                  _ -> error "can only map over arrays"))
  , definitionType =
      FunctionType
        (FunctionType ValueType ValueType)
        (FunctionType ValueType ValueType)
  }

--------------------------------------------------------------------------------
-- Function builders

arithmeticOperator :: Text -> (Scientific -> Scientific -> Scientific) -> Definition
arithmeticOperator name f =
  Definition
  { definitionName = Variable name
  , definitionCore =
      EvalCore
        (\x ->
           EvalCore
             (\y ->
                case (x, y) of
                  (ConstantCore (NumberConstant a), ConstantCore (NumberConstant b)) ->
                    ConstantCore (NumberConstant (f a b))
                  _ -> error ("type error for arguments to " <> show name)))
  , definitionType = ValueType .-> ValueType .-> ValueType
  , definitionDoc = "a " <> name <> " b"
  }

predicateOperator :: Text -> (Value -> Value -> Bool) -> Definition
predicateOperator name f =
  Definition
  { definitionName = Variable name
  , definitionCore =
      EvalCore
        (\x ->
           EvalCore
             (\y ->
                ConstantCore (BoolConstant (f (coreToValue x) (coreToValue y)))))
  , definitionType = ValueType .-> ValueType .-> ValueType
  , definitionDoc = "a " <> name <> " b"
  }

numericPredicateOperator :: Text -> (Scientific -> Scientific -> Bool) -> Definition
numericPredicateOperator name f =
  Definition
  { definitionName = Variable name
  , definitionCore =
      EvalCore
        (\x ->
           EvalCore
             (\y ->
                case (x, y) of
                  (ConstantCore (NumberConstant a), ConstantCore (NumberConstant b)) ->
                    ConstantCore (BoolConstant (f a b))
                  _ -> error ("type error for arguments to " <> show name)))
  , definitionType = ValueType .-> ValueType .-> ValueType
  , definitionDoc = "a " <> name <> " b"
  }

--------------------------------------------------------------------------------
-- Handy combinators

-- | Type a -> b.
(.->) :: Type -> Type -> Type
a .-> b = FunctionType a b
infixr 9 .->
