{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module : ExprDiff
Description : Contains a type class and instances for
differentiable expressions
Copyright : (c) Me (Kind of) @2018
Maintainer : zhouh46@mcmaster.ca
Stability : experimental
TODO : It contains some actions such as evaluation,
differentiation etc.
-}

module ExprDiff where

import           ExprType

import qualified Data.Map.Strict as Map

class DiffExpr a where
  -- | Evaluate an expression given var values
  eval :: Map.Map String a -> Expr a -> a
  -- | Simplify an expression and sub in values
  simplify :: Map.Map String a -> Expr a -> Expr a
  -- | Perform partial differention w.r.t identifier
  partDiff :: String -> Expr a -> Expr a
  -- | Add two Expression and simplify the result
  (!+) :: Expr a -> Expr a -> Expr a
  e1 !+ e2 = simplify (Map.fromList []) $ Add e1 e2
  -- | Multiply two Expression and simplify the result
  (!*) :: Expr a -> Expr a -> Expr a
  e1 !* e2 = simplify (Map.fromList []) $ Mult e1 e2
  -- | Apply Sin function to the expression
  sins :: Expr a -> Expr a
  sins e = simplify (Map.fromList []) $ Sin e
  -- | Apply Cos function to the expression
  coss :: Expr a -> Expr a
  coss e = simplify (Map.fromList []) $ Cos e
  -- | Apply exponential function to the expression
  exps :: Expr a -> Expr a
  exps e = simplify (Map.fromList []) $ Exp e
  -- | Parse values to expression datatype
  val :: a -> Expr a
  val x = Const x
  -- | Parse strings to expression datatype
  var :: String -> Expr a
  var x = Var x


instance (Floating a,Eq a) => DiffExpr a where
  eval vrs (Add e1 e2)  = eval vrs e1 + eval vrs e2
  eval vrs (Mult e1 e2) = eval vrs e1 * eval vrs e2
  eval vrs (Cos e)      = cos (eval vrs e)
  eval vrs (Sin e)      = sin (eval vrs e)
  eval vrs (Exp e)      = exp (eval vrs e)
  eval _ (Log e)        = log e
  eval vrs (Const x) = x
  eval vrs (Var x) = case Map.lookup x vrs of
                       Just v  -> v
                       Nothing -> error "failed lookup in eval"
  
  simplify vrs (Add e1 e2) = case s1 of
                                Const x1 -> case s2 of 
                                            Const x2 -> Const (x1 + x2)
                                            _        -> case x1 == 0 of
                                                          True -> s2
                                                          _    -> Add s1 s2
                                _        -> case s2 of 
                                            Const x2 -> case x2 == 0 of
                                                          True -> s1
                                                          _    -> Add s1 s2  
                                            _        -> Add s1 s2
                        where s1 = simplify vrs e1
                              s2 = simplify vrs e2
  
  simplify vrs (Mult e1 e2) = case s1 of
                                Const x1 -> case s2 of 
                                            Const x2 -> Const (x1 * x2)
                                            _        -> case x1 == 1 of
                                                          True -> s2
                                                          _    -> Mult s1 s2
                                _        -> case s2 of 
                                            Const x2 -> case x2 == 1 of
                                                          True -> s1
                                                          _    -> Mult s2 s1 
                                            _        -> Mult s1 s2
                        where s1 = simplify vrs e1
                              s2 = simplify vrs e2
  
  simplify vrs (Cos e)     = case s of
                                Const x -> Const (cos x)
                                _       -> Cos s
                        where s = simplify vrs e
  
  simplify vrs (Sin e)     = case s of
                                Const x -> Const (sin x)
                                _       -> Sin s
                        where s = simplify vrs e
  
  simplify vrs (Exp e)     = case s of
                                Const x -> Const (exp x)
                                _       -> Exp s
                        where s = simplify vrs e
  
  simplify _ (Log e)       = Const (log e)
  
  simplify vrs (Const x)   = Const x
  
  simplify vrs (Var x)     = case Map.lookup x vrs of
                       Just v  -> Const v
                       Nothing -> Var x
  
  partDiff x (Add e1 e2)  = (partDiff x e1) !+ (partDiff x e2)
  
  partDiff x (Mult (Const c) e2) = (Const c) !* (partDiff x e2)
  
  partDiff x (Mult e2 (Const c)) = (Const c) !* (partDiff x e2)
  
  partDiff x (Mult e1 e2) = (partDiff x e1) !* (partDiff x e2)
  
  partDiff x (Cos e)      = ((Const (-1)) !* (sins e)) !* (partDiff x e)
  
  partDiff x (Sin e)      = (coss e) !* (partDiff x e)
  
  partDiff x (Exp e)      = (exps e) !* (partDiff x e)
  
  partDiff _ (Log e)      = Const (log e)
  
  partDiff _ (Const x)    = Const 0
  
  partDiff x (Var y)      = case x == y of
                             True -> Const 1
                             _    -> Var y
  
