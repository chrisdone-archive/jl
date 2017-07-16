{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

-- |

module JL.Inferer where


import           Control.Monad.State.Strict
import           Control.Monad.Writer
import qualified Data.HashMap.Strict as HM
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           JL.Printer
import           JL.Types

-- | Get the type of the expression.
infer :: Map Variable Type -> Expression -> [TypeVariable] -> Type
infer ctx t stream =
  case evalState (check ctx t) stream of
    (ty, cs) ->
      let s = evalState (unify (S.toList cs)) ()
      in replace s ty

-- | Check the exp with the given context.
check
  :: MonadState ([TypeVariable]) m
  => Map Variable Type -> Expression -> m (Type, Set (Type, Type))
check ctx expr =
  case expr of
    VariableExpression name@(Variable text) ->
      case M.lookup name ctx of
        Nothing -> error ("Not in scope: `" <> T.unpack text <> "'")
        Just typ -> return (typ, mempty)
    LambdaExpression x xty body -> do
      (rty, cs) <- check (M.insert x xty ctx) body
      return (FunctionType xty rty, cs)
    ConstantExpression {} -> return (ValueType, mempty)
    ApplicationExpression f x -> do
      (fty, cs1) <- check ctx f
      (xty, cs2) <- check ctx x
      sym <- generateTypeVariable
      let rty = VariableType sym
          cs = S.insert (fty, FunctionType xty rty) (cs1 <> cs2)
      return (rty, cs)
    InfixExpression l f r -> do
      (fty, cs1) <- check ctx (VariableExpression f)
      (ty1, cs2) <- check ctx l
      (ty2, cs3) <- check ctx r
      sym <- generateTypeVariable
      let rty = VariableType sym
          cs =
            S.insert
              (fty, FunctionType ty1 (FunctionType ty2 rty))
              (cs1 <> cs2 <> cs3)
      return (rty, cs)
    IfExpression cond a b -> do
      (condty, cs1) <- check ctx cond
      (aty, cs2) <- check ctx a
      (bty, cs3) <- check ctx b
      sym <- generateTypeVariable
      let rty = VariableType sym
          cs =
            S.insert
              (condty, ValueType)
              (S.insert (aty, bty) (cs1 <> cs2 <> cs3))
      pure (rty, cs)
    RecordExpression pairs -> do
      cs <-
        foldM
          (\cs (_, e) -> do
             (pty, cs') <- check ctx e
             pure (S.insert (pty, ValueType) (cs <> cs')))
          mempty
          (HM.toList pairs)
      pure (ValueType, cs)
    SubscriptExpression e ks -> do
      (_, c1) <-
        (case e of
           ExpressionSubscripted es -> check ctx es
           WildcardSubscripted -> pure (ValueType, mempty))
      cs <-
        foldM
          (\cs s ->
             case s of
               PropertySubscript {} -> pure cs
               ExpressionSubscript es -> do
                 (pty, cs') <- check ctx es
                 pure (S.insert (pty, ValueType) (cs <> cs')))
          c1
          ks
      sym <- generateTypeVariable
      let rty = VariableType sym
      pure (rty, cs)
    ArrayExpression as -> do
      cs <-
        foldM
          (\cs e -> do
             (pty, cs') <- check ctx e
             pure (S.insert (pty, ValueType) (cs <> cs')))
          mempty
          as
      pure (ValueType, cs)

-- | Generate a fresh type variable.
generateTypeVariable
  :: MonadState ([TypeVariable]) m
  => m TypeVariable
generateTypeVariable = do
  v:vs <- get
  put vs
  pure v

-- | Unify the list of constraints.
unify
  :: Monad m
  => [(Type, Type)] -> m (Map TypeVariable Type)
unify [] = return mempty
unify ((a, b):cs)
  | a == b = unify cs
  | VariableType v <- a = unifyVariable v cs a b
  | VariableType v <- b = unifyVariable v cs b a
  | FunctionType a1 b1 <- a
  , FunctionType a2 b2 <- b = unify ([(a1, a2), (b1, b2)] <> cs)
  | otherwise =
    error
      (T.unpack
         ("Type " <> quote (prettyType a) <> " doesn't match " <>
          quote (prettyType b)))

-- | Unify a type variable for two types.
unifyVariable
  :: Monad m
  => TypeVariable -> [(Type, Type)] -> Type -> Type -> m (Map TypeVariable Type)
unifyVariable v cs a b =
  if occurs v b
    then error
           (T.unpack ("Occurs check: " <> prettyType a <> " ~ " <> prettyType b))
    else let subbed = M.singleton v b
         in do rest <- unify (substitute subbed cs)
               return (rest <> subbed)

-- | Occurs check.
occurs :: TypeVariable -> Type -> Bool
occurs x (VariableType y)
  | x == y = True
  | otherwise = False
occurs x (FunctionType a b) = occurs x a || occurs x b
occurs _ ValueType = False

-- | Substitute the unified type into the constraints.
substitute :: Map TypeVariable Type -> [(Type, Type)] -> [(Type, Type)]
substitute subs = map go
  where
    go (a, b) = (replace subs a, replace subs b)

-- | Apply a substitution to a type.
replace :: Map TypeVariable Type -> Type -> Type
replace s' t' = M.foldrWithKey go t' s'
  where
    go s1 t (VariableType s2)
      | s1 == s2 = t
      | otherwise = VariableType s2
    go s t (FunctionType t2 t3) = FunctionType (go s t t2) (go s t t3)
    go _ _ ValueType = ValueType

-- | Quote something for showing to the programmer.
quote :: Text -> Text
quote t = "‘" <> t <> "’"
