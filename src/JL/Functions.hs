{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |

module JL.Functions (context, scope, functions) where

import           Control.Arrow
import           Control.Monad.Writer
import           Data.Aeson (Value)
import           Data.Function
import qualified Data.HashMap.Strict as HM
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Ord
import           Data.Scientific
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import           JL.Interpreter
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
  [ ("Record access", [getf, setf, modifyf, keysf, elemsf])
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
      , anyf
      , allf
      , nubf
      , sortf
      , appendf
      ,  sumf, minimumf, maximumf
      ])
  , ( "Predicate operators"
    , [predicateOperator "/=" (/=), predicateOperator "=" (==)])
  , ( "Boolean operators"
    , [boolOperator "&&" (&&), boolOperator "||" (||), boolFun "not" not])
  , ( "Numeric operators"
    , [ numericPredicateOperator ">" (>)
      , numericPredicateOperator "<" (<)
      , numericPredicateOperator ">=" (>=)
      , numericPredicateOperator "<=" (<=)
      , arithmeticOperator "*" (*)
      , arithmeticOperator "+" (+)
      , arithmeticOperator "-" (-)
      , arithmeticOperator "/" (/)
      , arithmeticOperator "min" min
      , arithmeticOperator "max" max
      , arithmeticFun "abs" abs
      ])
  , ("Function combinators", [idf, compose, flipf])
  ]

--------------------------------------------------------------------------------
-- Functions

keysf :: Definition
keysf =
  Definition
  { definitionDoc = "Get all keys of the object"
  , definitionName = Variable "keys"
  , definitionCore =
      (EvalCore
         (\obj ->
            case obj of
              RecordCore o ->
                ArrayCore
                  (V.fromList (map (ConstantCore . StringConstant) (HM.keys o)))
              _ -> error "keys function expected an object"))
  , definitionType = FunctionType JSONType JSONType
  }

elemsf :: Definition
elemsf =
  Definition
  { definitionDoc = "Get all elements of the object"
  , definitionName = Variable "elems"
  , definitionCore =
      (EvalCore
         (\obj ->
            case obj of
              RecordCore o ->
                ArrayCore
                  (V.fromList (HM.elems o))
              _ -> error "elems function expected an object"))
  , definitionType = FunctionType JSONType JSONType
  }

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
        JSONType
        (FunctionType
           (FunctionType JSONType JSONType)
           (FunctionType JSONType JSONType))
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
                   _ -> error "type error for get arguments")))
  , definitionType = FunctionType JSONType (FunctionType JSONType JSONType)
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
        JSONType
        (FunctionType JSONType (FunctionType JSONType JSONType))
  }

idf :: Definition
idf =
  Definition
  { definitionDoc = "Identity function, returns its input unchanged"
  , definitionName = Variable "id"
  , definitionCore = (EvalCore (\x -> x))
  , definitionType = FunctionType JSONType JSONType
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
        (FunctionType JSONType (FunctionType JSONType JSONType))
        (FunctionType JSONType (FunctionType JSONType JSONType))
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
        (FunctionType JSONType JSONType)
        (FunctionType
           (FunctionType JSONType JSONType)
           (FunctionType JSONType JSONType))
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
                       ConstantCore (StringConstant xs') ->
                         (T.foldl
                            (\acc x ->
                               eval
                                 (ApplicationCore
                                    (ApplicationCore cons acc)
                                    (ConstantCore
                                       (StringConstant (T.singleton x)))))
                            nil
                            xs')
                       _ -> error "can only fold sequences")))
  , definitionType =
      (FunctionType
         ((FunctionType JSONType (FunctionType JSONType JSONType)))
         (FunctionType JSONType (FunctionType JSONType JSONType)))
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
        (FunctionType JSONType (FunctionType JSONType JSONType))
        (FunctionType JSONType (FunctionType JSONType JSONType))
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
                  (ConstantCore (StringConstant xs')) ->
                    (ConstantCore
                       (BoolConstant
                          (isJust
                             (T.findIndex
                                (\cc ->
                                   case n of
                                     ConstantCore (StringConstant c) ->
                                       c == T.singleton cc
                                     _ -> False)
                                xs'))))
                  _ ->
                    error
                      "can only check elements from sequences"))
  , definitionType = FunctionType JSONType (FunctionType JSONType JSONType)
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
                  (ConstantCore (NumberConstant n'), ConstantCore (StringConstant xs')) ->
                    (ConstantCore (StringConstant (T.take (round n') xs')))
                  _ -> error "can only take from sequences"))
  , definitionType = FunctionType JSONType (FunctionType JSONType JSONType)
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
                  (ConstantCore (NumberConstant n'), ConstantCore (StringConstant xs')) ->
                    (ConstantCore (StringConstant (T.drop (round n') xs')))
                  (ConstantCore (NumberConstant n'), ArrayCore xs') ->
                    (ArrayCore (V.drop (round n') xs'))
                  _ -> error "can only drop from sequences"))
  , definitionType = FunctionType JSONType (FunctionType JSONType JSONType)
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
  , definitionType = FunctionType JSONType JSONType
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
              (ConstantCore (StringConstant xs')) ->
                (ConstantCore (StringConstant (T.reverse xs')))
              _ -> error "can only reverse a sequence"))
  , definitionType = FunctionType JSONType JSONType
  }

appendf :: Definition
appendf =
  Definition
  { definitionDoc = "Append a sequence"
  , definitionName = Variable "append"
  , definitionCore =
      (EvalCore
         (\xs ->
            (EvalCore
               (\ys ->
                  case (xs, ys) of
                    (ArrayCore xs', ArrayCore ys') -> (ArrayCore (xs' <> ys'))
                    (ConstantCore (StringConstant xs'), ConstantCore (StringConstant ys')) ->
                      (ConstantCore (StringConstant (xs' <> ys')))
                    _ -> error "can only append two sequences of the same type"))))
  , definitionType = FunctionType JSONType JSONType
  }

nubf :: Definition
nubf =
  Definition
  { definitionDoc = "Return the sequence with no duplicates; the nub of it"
  , definitionName = Variable "nub"
  , definitionCore =
      (EvalCore
         (\xs ->
            case xs of
              (ArrayCore xs') ->
                (ArrayCore
                   (V.fromList (nubBy (on (==) coreToValue) (V.toList xs'))))
              (ConstantCore (StringConstant xs')) ->
                (ConstantCore
                   (StringConstant
                      (T.pack (nub (T.unpack xs')))))
              _ -> error "can only nub a sequence"))
  , definitionType = FunctionType JSONType JSONType
  }

sortf :: Definition
sortf =
  Definition
  { definitionDoc = "Return the sequence sorted"
  , definitionName = Variable "sort"
  , definitionCore =
      (EvalCore
         (\xs ->
            case xs of
              (ArrayCore xs') ->
                (ArrayCore
                   (V.fromList (sortBy (comparing coreToCompare) (V.toList xs'))))
              (ConstantCore (StringConstant xs')) ->
                (ConstantCore
                   (StringConstant
                      (T.pack (sort (T.unpack xs')))))
              _ -> error "can only sort a sequence"))
  , definitionType = FunctionType JSONType JSONType
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
              (ConstantCore (StringConstant xs')) ->
                (ConstantCore (NumberConstant (fromIntegral (T.length xs'))))
              _ -> error "can only take length of sequences"))
  , definitionType = FunctionType JSONType JSONType
  }

sumf :: Definition
sumf =
  Definition
  { definitionDoc = "Get the sum of a sequence"
  , definitionName = Variable "sum"
  , definitionCore =
      (EvalCore
         (\xs ->
            case xs of
              (ArrayCore xs') ->
                (ConstantCore (NumberConstant (V.sum (fmap coreToNumber xs'))))
              _ -> error "can only take sum of arrays"))
  , definitionType = FunctionType JSONType JSONType
  }

productf :: Definition
productf =
  Definition
  { definitionDoc = "Get the product of a sequence"
  , definitionName = Variable "product"
  , definitionCore =
      (EvalCore
         (\xs ->
            case xs of
              (ArrayCore xs') ->
                (ConstantCore (NumberConstant (V.product (fmap coreToNumber xs'))))
              _ -> error "can only take product of arrays"))
  , definitionType = FunctionType JSONType JSONType
  }

maximumf :: Definition
maximumf =
  Definition
  { definitionDoc = "Get the maximum of a sequence"
  , definitionName = Variable "maximum"
  , definitionCore =
      (EvalCore
         (\xs ->
            case xs of
              (ArrayCore xs') ->
                (ConstantCore (NumberConstant (V.maximum (fmap coreToNumber xs'))))
              _ -> error "can only take maximum of arrays"))
  , definitionType = FunctionType JSONType JSONType
  }

minimumf :: Definition
minimumf =
  Definition
  { definitionDoc = "Get the minimum of a sequence"
  , definitionName = Variable "minimum"
  , definitionCore =
      (EvalCore
         (\xs ->
            case xs of
              (ArrayCore xs') ->
                (ConstantCore (NumberConstant (V.minimum (fmap coreToNumber xs'))))
              _ -> error "can only take minimum of arrays"))
  , definitionType = FunctionType JSONType JSONType
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
              (ConstantCore (StringConstant xs')) ->
                (ConstantCore (BoolConstant (T.null xs')))
              _ -> error "can only check if sequences are empty"))
  , definitionType = FunctionType JSONType JSONType
  }

anyf :: Definition
anyf =
  Definition
  { definitionDoc = "Does p return true for any of the elements?"
  , definitionName = Variable "any"
  , definitionCore =
      EvalCore
        (\f ->
           EvalCore
             (\xs ->
                case xs of
                  (ArrayCore xs') ->
                    (ConstantCore
                       (BoolConstant
                          (V.any
                             (\x ->
                                case eval (ApplicationCore f x) of
                                  ConstantCore (BoolConstant b) -> b
                                  _ -> True)
                             xs')))
                  (ConstantCore (StringConstant xs')) ->
                    (ConstantCore
                       (BoolConstant
                          (T.any
                             (\x ->
                                case eval
                                       (ApplicationCore
                                          f
                                          (ConstantCore
                                             (StringConstant (T.singleton x)))) of
                                  ConstantCore (BoolConstant b) -> b
                                  _ -> True)
                             xs')))
                  _ -> error "can only any over sequences"))
  , definitionType =
      FunctionType
        (FunctionType JSONType JSONType)
        (FunctionType JSONType JSONType)
  }

allf :: Definition
allf =
  Definition
  { definitionDoc = "Does p return true for all of the elements?"
  , definitionName = Variable "all"
  , definitionCore =
      EvalCore
        (\f ->
           EvalCore
             (\xs ->
                case xs of
                  (ArrayCore xs') ->
                    (ConstantCore
                       (BoolConstant
                          (V.all
                             (\x ->
                                case eval (ApplicationCore f x) of
                                  ConstantCore (BoolConstant b) -> b
                                  _ -> True)
                             xs')))
                  (ConstantCore (StringConstant xs')) ->
                    (ConstantCore
                       (BoolConstant
                          (T.all
                             (\x ->
                                case eval
                                       (ApplicationCore
                                          f
                                          (ConstantCore
                                             (StringConstant (T.singleton x)))) of
                                  ConstantCore (BoolConstant b) -> b
                                  _ -> True)
                             xs')))
                  _ -> error "can only all over sequences"))
  , definitionType =
      FunctionType
        (FunctionType JSONType JSONType)
        (FunctionType JSONType JSONType)
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
                  (ConstantCore (StringConstant xs')) ->
                    (ConstantCore
                       (StringConstant
                          (T.dropWhile
                             (\x ->
                                case eval
                                       (ApplicationCore
                                          f
                                          (ConstantCore
                                             (StringConstant (T.singleton x)))) of
                                  ConstantCore (BoolConstant b) -> b
                                  _ -> True)
                             xs')))
                  _ -> error "can only dropWhile over sequences"))
  , definitionType =
      FunctionType
        (FunctionType JSONType JSONType)
        (FunctionType JSONType JSONType)
  }

takeWhilef :: Definition
takeWhilef =
  Definition
  { definitionDoc =
      "Take elements from a sequence while given predicate is true"
  , definitionName = Variable "takeWhile"
  , definitionCore =
      (EvalCore
         (\f ->
            EvalCore
              (\xs ->
                 case xs of
                   (ConstantCore (StringConstant xs')) ->
                     (ConstantCore
                        (StringConstant
                           (T.takeWhile
                              (\x ->
                                 case eval
                                        (ApplicationCore
                                           f
                                           (ConstantCore
                                              (StringConstant (T.singleton x)))) of
                                   ConstantCore (BoolConstant b) -> b
                                   _ -> True)
                              xs')))
                   (ArrayCore xs') ->
                     (ArrayCore
                        (V.takeWhile
                           (\x ->
                              case eval (ApplicationCore f x) of
                                ConstantCore (BoolConstant b) -> b
                                _ -> True)
                           xs'))
                   _ -> error "can only takeWhile over sequences")))
  , definitionType =
      FunctionType
        (FunctionType JSONType JSONType)
        (FunctionType JSONType JSONType)
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
                  (ConstantCore (StringConstant xs')) ->
                    (ConstantCore
                       (StringConstant
                          (T.filter
                             (\x ->
                                case eval
                                       (ApplicationCore
                                          f
                                          (ConstantCore
                                             (StringConstant (T.singleton x)))) of
                                  ConstantCore (BoolConstant b) -> b
                                  _ -> True)
                             xs')))
                  (ArrayCore xs') ->
                    (ArrayCore
                       (V.filter
                          (\x ->
                             case eval (ApplicationCore f x) of
                               ConstantCore (BoolConstant b) -> b
                               _ -> True)
                          xs'))
                  _ -> error "can only filter over sequences"))
  , definitionType =
      FunctionType
        (FunctionType JSONType JSONType)
        (FunctionType JSONType JSONType)
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
                  (ConstantCore (StringConstant xs')) ->
                    (ConstantCore
                       (StringConstant
                          (T.concatMap
                             (\x ->
                                case eval
                                       (ApplicationCore
                                          f
                                          (ConstantCore
                                             (StringConstant (T.singleton x)))) of
                                  ConstantCore (StringConstant b) -> b
                                  _ -> error "map over a string must return strings")
                             xs')))
                  (ArrayCore xs') ->
                    (ArrayCore (fmap (\x -> (eval (ApplicationCore f (x)))) xs'))
                  _ -> error "can only map over sequences"))
  , definitionType =
      FunctionType
        (FunctionType JSONType JSONType)
        (FunctionType JSONType JSONType)
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
  , definitionType = JSONType .-> JSONType .-> JSONType
  , definitionDoc = "a " <> name <> " b"
  }

boolOperator :: Text -> (Bool -> Bool -> Bool) -> Definition
boolOperator name f =
  Definition
  { definitionName = Variable name
  , definitionCore =
      EvalCore
        (\x ->
           EvalCore
             (\y ->
                case (x, y) of
                  (ConstantCore (BoolConstant a), ConstantCore (BoolConstant b)) ->
                    ConstantCore (BoolConstant (f a b))
                  _ -> error ("type error for arguments to " <> show name)))
  , definitionType = JSONType .-> JSONType .-> JSONType
  , definitionDoc = "a " <> name <> " b"
  }

arithmeticFun :: Text -> (Scientific -> Scientific) -> Definition
arithmeticFun name f =
  Definition
  { definitionName = Variable name
  , definitionCore =
      EvalCore
        (\x ->
           case (x) of
             (ConstantCore (NumberConstant a)) ->
               ConstantCore (NumberConstant (f a))
             _ -> error ("type error for arguments to " <> show name))
  , definitionType = JSONType .-> JSONType
  , definitionDoc = name <> " b"
  }

boolFun :: Text -> (Bool -> Bool) -> Definition
boolFun name f =
  Definition
  { definitionName = Variable name
  , definitionCore =
      EvalCore
        (\x ->
           case (x) of
             (ConstantCore (BoolConstant a)) ->
               ConstantCore (BoolConstant (f a))
             _ -> error ("type error for arguments to " <> show name))
  , definitionType = JSONType .-> JSONType
  , definitionDoc = name <> " b"
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
  , definitionType = JSONType .-> JSONType .-> JSONType
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
  , definitionType = JSONType .-> JSONType .-> JSONType
  , definitionDoc = "a " <> name <> " b"
  }

--------------------------------------------------------------------------------
-- Handy combinators

-- | Type a -> b.
(.->) :: Type -> Type -> Type
a .-> b = FunctionType a b
infixr 9 .->
