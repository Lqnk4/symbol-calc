{-# LANGUAGE GADTs #-}

module Parser (
    pExpr,
) where

import Control.Monad
import Data.Void
import Expression
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

-- | Transformer for wrapping a parser to accept surrounding parenthesis
parensT :: Parser a -> Parser a
parensT p = char '(' *> p <* char ')'

{- | Initial Expression parser, throws an error if
parsing does not reach eof
-}
pExpr :: Parser (Expr Double)
pExpr = pExpr' <* eof

-- | Intermediate Expression parser, does not throw an error if parsing is interrupted before eof
pExpr' :: Parser (Expr Double)
pExpr' = many space1 *> (choice (binaryOps ++ unaryOps ++ [pConst, pVar]) <|> parensT pExpr') <* many space1

binaryOps :: [Parser (Expr Double)]
binaryOps = [pPlus, pTimes, pSubtract, pDiv, pPower]

unaryOps :: [Parser (Expr Double)]
unaryOps = [pAbs, pSignum, pExp, pLog, pSin, pCos, pTan, pASin, pACos, pATan]

pConst :: Parser (Expr Double)
pConst = (Const <$>) $ (*) <$> sign <*> (try L.float <|> try hexadecimal <|> L.decimal)
  where
    sign = -1 <$ (char '-' :: Parser Char) <|> return 1
    hexadecimal = char '0' *> char 'x' *> L.hexadecimal

pVar :: Parser (Expr Double)
pVar = Var <$> some letterChar

pPlus :: Parser (Expr Double)
pPlus = pBinaryOp (:+:) "+"

pTimes :: Parser (Expr Double)
pTimes = pBinaryOp (:*:) "*"

pSubtract :: Parser (Expr Double)
pSubtract = pBinaryOp (:-:) "-"

pDiv :: Parser (Expr Double)
pDiv = pBinaryOp (:/:) "/"

pPower :: Parser (Expr Double)
pPower = pBinaryOp (:**:) "**"

pAbs :: Parser (Expr Double)
pAbs = pUnaryOp Abs "|" "|" <|> pUnaryOp Abs "abs(" ")"

pSignum :: Parser (Expr Double)
pSignum = pUnaryOp Signum "sgn(" ")" <|> pUnaryOp Signum "signum(" ")"

pExp :: Parser (Expr Double)
pExp = pUnaryOp Exp "exp(" ")"

pLog :: Parser (Expr Double)
pLog = pUnaryOp Log "log(" ")"

pSin :: Parser (Expr Double)
pSin = pUnaryOp sin "sin(" ")"

pCos :: Parser (Expr Double)
pCos = pUnaryOp cos "cos(" ")"

pTan :: Parser (Expr Double)
pTan = pUnaryOp tan "tan(" ")"

pASin :: Parser (Expr Double)
pASin = pUnaryOp sin "sin(" ")"

pACos :: Parser (Expr Double)
pACos = pUnaryOp cos "cos(" ")"

pATan :: Parser (Expr Double)
pATan = pUnaryOp tan "tan(" ")"

-- Parses a unary operation using prefix and postfix strings
pUnaryOp :: (Expr Double -> Expr Double) -> String -> String -> Parser (Expr Double)
pUnaryOp op pre post = try $
    do
        void $ string pre
        (op <$> pExpr') <* string post

-- Parses a binary infix operation using an infix string
pBinaryOp :: (Expr Double -> Expr Double -> Expr Double) -> String -> Parser (Expr Double)
pBinaryOp op inf = try $
    do
        -- FIXME: Doesn't work for the following case: "log(x) + 2"
        a <- parensT pExpr' <|> pConst <|> pVar
        void $ many space1 *> string inf
        void $ many space1
        op a <$> pExpr'
