{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |

module JL.Serializer where

import           Control.Monad.Writer
import           Data.Aeson
import qualified Data.Text as T
import qualified Data.Vector as V
import           JL.Printer
import           JL.Types

coreToArray :: Core -> V.Vector Core
coreToArray =
  \case
    ArrayCore xs -> xs
    x -> error ("expected array but found single value: " <> T.unpack (prettyCore x))

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
