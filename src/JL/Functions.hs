{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |

module JL.Functions where

import           Control.Monad.Writer
import qualified Data.HashMap.Strict as HM
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V
import           JL.Interpreter
import           JL.Printer
import           JL.Serializer
import           JL.Types

context :: Map Variable Type
context = fmap snd bindings

scope :: Map Variable Core
scope = fmap fst bindings

bindings :: Map Variable (Core, Type)
bindings = M.fromList (concat [arith, records, arrays, funcs])

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

funcs :: [(Variable, (Core, Type))]
funcs = [idf, compose, flipf]
  where
    idf =
      (Variable "id", (EvalCore (\x -> x), FunctionType ValueType ValueType))
    flipf =
      ( Variable "flip"
      , ( EvalCore
            (\f ->
               EvalCore
                 (\x ->
                    EvalCore
                      (\y -> eval (ApplicationCore (ApplicationCore f y) x))))
        , FunctionType
            (FunctionType ValueType (FunctionType ValueType ValueType))
            (FunctionType ValueType (FunctionType ValueType ValueType))))
    compose =
      ( Variable "compose"
      , ( EvalCore
            (\f ->
               EvalCore
                 (\g ->
                    EvalCore
                      (\x -> eval (ApplicationCore g (ApplicationCore f x)))))
        , FunctionType
            (FunctionType ValueType ValueType)
            (FunctionType
               (FunctionType ValueType ValueType)
               (FunctionType ValueType ValueType))))

arrays :: [(Variable, (Core, Type))]
arrays =
  [ mapf
  , filterf
  , len
  , takef
  , dropf
  , empty
  , concatf
  , rev
  , zipw
  , takeWhilef
  , dropWhilef
  , elemf
  , foldf
  ]
  where
    foldf =
      ( Variable "fold"
      , ( EvalCore
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
                                     (ApplicationCore
                                        (ApplicationCore cons acc)
                                        x))
                                nil
                                xs')
                           _ -> error "can only fold arrays")))
        , (FunctionType (FunctionType ValueType (FunctionType ValueType ValueType))
                        (FunctionType ValueType (FunctionType ValueType ValueType)))))
    zipw =
      ( Variable "zipWith"
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
                                      eval
                                        (ApplicationCore (ApplicationCore f x) y))
                                   xs'
                                   ys'))
                           _ -> error "can only zip two arrays")))
        , FunctionType
            (FunctionType ValueType (FunctionType ValueType ValueType))
            (FunctionType ValueType (FunctionType ValueType ValueType))))
    elemf =
      ( Variable "elem"
      , ( EvalCore
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
                  _ -> error "can only concat arrays"))
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
    dropWhilef =
      ( Variable "dropWhile"
      , ( EvalCore
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
        , FunctionType
            (FunctionType ValueType ValueType)
            (FunctionType ValueType ValueType)))
    takeWhilef =
      ( Variable "takeWhile"
      , ( EvalCore
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
                      _ -> error "can only takeWhile over arrays"))
        , FunctionType
            (FunctionType ValueType ValueType)
            (FunctionType ValueType ValueType)))
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

--------------------------------------------------------------------------------
-- Handy combinators

-- | Type a -> b.
(.->) :: Type -> Type -> Type
a .-> b = FunctionType a b
infixr .->

-- | Type signature f :: t.
(.::) :: t1 -> t -> (t1, t)
f .:: t = (f, t)
infixl .::
