{-|
Module : ExprParser
Description : Just the Parser of Expr
Copyright : (c) Me (Kind of) @2018
Maintainer : zhouh46@mcmaster.ca
Stability : experimental
-}

module ExprParser (parseExpr) where

import           ExprType

import           Text.Parsec
import           Text.Parsec.String

parseExpr :: String -> Expr Double
parseExpr = parseExprD


-- | Parses a string into an Expr Double type
parseExprD :: String -> Expr Double
parseExprD ss = case parse exprD "" ss of
                  Left err   -> error $ show err
                  Right expr -> expr


-- | Parses a string into an Expr Float type
parseExprF :: String -> Expr Float
parseExprF ss = case parse exprF "" ss of
                  Left err   -> error $ show err
                  Right expr -> expr

exprD :: Parser (Expr Double)
exprD = termMD

exprF :: Parser (Expr Float)
exprF = termMF

keywordSin  = try (do{ string "sin" ; notFollowedBy varName})
keywordCos  = try (do{ string "cos" ; notFollowedBy varName})
keywordExp  = try (do{ string "exp" ; notFollowedBy varName})
keywordLog  = try (do{ string "log" ; notFollowedBy varName})

token_ :: Parser a -> Parser a
token_ p = do {spaces; a <- p; spaces ; return a}

reserved :: String -> Parser String
reserved s = token_ (string s)

varName :: Parser String
varName = do{head <- letter;tail <- many (letter <|> digit);return (head:tail)}

parens :: Parser a -> Parser a
parens m = do{reserved "(";n <- m;reserved ")";return n}

numberPos :: Parser String
numberPos = many1 digit

numberNeg :: Parser String
numberNeg = do {char '-';n <- numberPos; return ("-"++n)}

decimalPos :: Parser String
decimalPos = do {n <- numberPos; char '.';d <- numberPos;return (n++"."++d)}

decimalNeg :: Parser String
decimalNeg = do {char '-';n <- numberPos; char '.';d <- numberPos;return ("-"++n++"."++d)}

decimal :: Parser String
decimal = try decimalPos <|> try decimalNeg <|> try numberPos <|> try numberNeg

float :: Parser Float
float = read <$> decimal

double :: Parser Double
double = read <$> decimal

var :: Parser (Expr a)
var = Var <$> token_ varName

plusOp = token_ (char '+') >> return Add
multOp = token_ (char '*') >> return Mult

constF :: Parser (Expr Float)
constF = Const <$> float

termMF :: Parser (Expr Float)
termMF = termMHF <|> parens termMHF

termMHF :: Parser (Expr Float)
termMHF = termPF `chainl1` plusOp

termPF :: Parser (Expr Float)
termPF = termPHF <|> parens termPHF

termPHF :: Parser (Expr Float)
termPHF = atomF `chainl1` multOp

atomF :: Parser (Expr Float)
atomF = atomHF <|> parens atomHF

atomHF :: Parser (Expr Float)
atomHF = sinF <|> cosF <|> expF <|> logF <|> constF <|> var 

sinF :: Parser (Expr Float)
sinF = do { token_ keywordSin;reserved "(";e <- exprF ;reserved ")";return (Sin e)}

cosF :: Parser (Expr Float)
cosF = do { token_ keywordCos;reserved "(";e <- exprF ;reserved ")";return (Cos e)}

expF :: Parser (Expr Float)
expF = do { token_ keywordExp;reserved "(";e <- exprF ;reserved ")";return (Exp e)}

logF :: Parser (Expr Float)
logF = do { token_ keywordLog;reserved "(";e <- float ;reserved ")";return (Log e)}

-------------------------------------------------------------------------------------------------------------

constD :: Parser (Expr Double)
constD = Const <$> double

termMD :: Parser (Expr Double)
termMD = termMHD <|> parens termMHD

termMHD :: Parser (Expr Double)
termMHD = termPD `chainl1` plusOp

termPD :: Parser (Expr Double)
termPD = termPHD <|> parens termPHD

termPHD :: Parser (Expr Double)
termPHD = atomD `chainl1` multOp

atomD :: Parser (Expr Double)
atomD = atomHD <|> parens atomHD

atomHD :: Parser (Expr Double)
atomHD = sinD <|> cosD <|> expD <|> logD <|> constD <|> var 

sinD :: Parser (Expr Double)
sinD = do { token_ keywordSin;reserved "(";e <- exprD ;reserved ")";return (Sin e)}

cosD :: Parser (Expr Double)
cosD = do { token_ keywordCos;reserved "(";e <- exprD ;reserved ")";return (Cos e)}

expD :: Parser (Expr Double)
expD = do { token_ keywordExp;reserved "(";e <- exprD ;reserved ")";return (Exp e)}

logD :: Parser (Expr Double)
logD = do { token_ keywordLog;reserved "(";e <- double ;reserved ")";return (Log e)}

