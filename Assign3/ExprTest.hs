{-|
Module : ExprTest
Description : Contains main function and do the
calculation based on other modules and prints
the result
Copyright : (c) Me (Kind of) @2018
Maintainer : zhouh46@mcmaster.ca
Stability : experimental
TODO : Just to calculate and output the 
result based on other 4 modules
-}

module ExprTest where

import           ExprDiff
import           ExprParser
import           ExprPretty
import           ExprType

import qualified Data.Map.Strict as Map
import           Test.QuickCheck

main :: IO()
main = do
        putStrLn "Please input a expression:"
        exprstr <- getLine
        let expr = parseExpr exprstr
        putStrLn "Pretty print:"
        putStrLn (show expr)
        putStrLn "Simplify:"
        putStrLn (show (simplify (Map.fromList []) expr))
        putStrLn "Diff for x:"
        putStrLn (show (partDiff "x" expr))
        putStrLn "Eval:"
        case simplify (Map.fromList []) expr of  
           Const x -> putStrLn (show (eval (Map.fromList []) expr))
           _       -> putStrLn "There is an unknown in the form and cannot be evaluated!"

         
