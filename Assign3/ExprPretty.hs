{-|
Module : ExprPretty
Description : Convert Expr into general form
which makes it much easier to read
Copyright : (c) Me (Kind of) @2018
Maintainer : zhouh46@mcmaster.ca
Stability : experimental
-}

module ExprPretty where

import           ExprType

parens :: String -> String
parens ss = "(" ++ ss ++ ")"

instance Show a => Show (Expr a) where
  show (Mult e1 e2) = case e1 of
               Add _ _  -> case e2 of 
                            Add _ _ -> parens (show e1) ++ "*" ++ parens (show e2)
                            _       -> parens (show e1) ++ "*" ++ (show e2)
               _        -> case e2 of 
                            Add _ _ -> (show e1) ++ "*" ++ parens (show e2)
                            _       -> (show e1) ++ "*" ++ (show e2)
  show (Add e1 e2)  = (show e1) ++ "+" ++ (show e2)
  show (Cos ss)     = "cos" ++ parens (show ss)
  show (Sin ss)     = "sin" ++ parens (show ss)
  show (Exp ss)     = "exp" ++ parens (show ss)
  show (Log ss)     = "exp" ++ parens (show ss)
  show (Var ss)     = ss
  show (Const x)    = show x
  
