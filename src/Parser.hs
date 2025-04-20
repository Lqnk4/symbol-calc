{-# LANGUAGE GADTs #-}

module Parser where

import Control.Monad
import Data.Void
import Expression
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

pExpr :: Parser (Expr Double)
pExpr = choice $ binParsers ++ [pConst, pVar]

pConst :: Parser (Expr Double)
pConst = (Const <$>) $ (*) <$> try sign <*> L.float
  where
    sign = -1 <$ (char '-' :: Parser Char) <|> return 1

pVar :: Parser (Expr Double)
pVar = Var <$> some alphaNumChar

pPlus :: Parser (Expr Double)
pPlus = pBinaryOp (:+:) "" "+" ""

pTimes :: Parser (Expr Double)
pTimes = pBinaryOp (:*:) "" "*" ""

pSubtract :: Parser (Expr Double)
pSubtract = pBinaryOp (:-:) "" "-" ""

pDiv :: Parser (Expr Double)
pDiv = pBinaryOp (:/:) "" "/" ""

binParsers :: [Parser (Expr Double)]
binParsers = [pPlus, pTimes, pSubtract, pDiv]

pBinaryOp :: (Expr Double -> Expr Double -> Expr Double) -> String -> String -> String -> Parser (Expr Double)
pBinaryOp op pre inf post =
    try
        ( do
            void $ string pre
            a <- char '(' *> pExpr <* char ')'
            void $ string inf
            b <- char '(' *> pExpr <* char ')'
            void $ string post
            return (op a b)
        )
