{-|
Module : ExprType
Description : Contains all the definitions of every datatypes
Copyright : (c) Me (Kind of) @2018
Maintainer : zhouh46@mcmaster.ca
Stability : experimental
-}

module ExprType where

import           Data.List

data Expr a = Add (Expr a) (Expr a)     -- ^ Binary Addition
            | Mult (Expr a) (Expr a)    -- ^ Bianry Multiplication
            | Cos (Expr a)              -- ^ Cosine Function
            | Sin (Expr a)              -- ^ Sine Function
            | Log a                     -- ^ Log Function
            | Exp (Expr a)              -- ^ Exponent Function
            | Const a                   -- ^ Value Wrapper
            | Var String                -- ^ String Wrapper
  deriving (Eq)

getVars :: Expr a -> [String]
getVars (Add e1 e2)  = getVars e1 `union` getVars e2
getVars (Mult e1 e2) = getVars e1 `union` getVars e2
getVars (Cos e)      = getVars e
getVars (Sin e)      = getVars e
getVars (Exp e)      = getVars e
getVars (Const _)    = []
getVars (Var ident)  = [ident]
