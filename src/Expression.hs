{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ViewPatterns #-}

module Expression (
    Expr,
    eval,
    subVar,
) where

import Data.Complex (Complex, imagPart, realPart)
import Data.Kind (Type)

infixl 4 :+:, :-:
infixl 5 :*:, :/:
infixr 6 :**:

-- | Symbolic Expressions
data Expr :: (Type -> Type) where
    -- Literal
    Const :: a -> Expr a
    Var :: String -> Expr a
    -- Num
    (:+:) :: (Num a) => Expr a -> Expr a -> Expr a
    (:-:) :: (Num a) => Expr a -> Expr a -> Expr a
    (:*:) :: (Num a) => Expr a -> Expr a -> Expr a
    Abs :: (Num a) => Expr a -> Expr a
    Signum :: (Num a) => Expr a -> Expr a
    Negate :: (Num a) => Expr a -> Expr a
    -- Fractional
    (:/:) :: (Fractional a) => Expr a -> Expr a -> Expr a
    -- Floating
    (:**:) :: (Floating a) => Expr a -> Expr a -> Expr a
    Exp :: (Floating a) => Expr a -> Expr a
    Log :: (Floating a) => Expr a -> Expr a
    Sin :: (Floating a) => Expr a -> Expr a
    Cos :: (Floating a) => Expr a -> Expr a
    Tan :: (Floating a) => Expr a -> Expr a
    ASin :: (Floating a) => Expr a -> Expr a
    ACos :: (Floating a) => Expr a -> Expr a
    ATan :: (Floating a) => Expr a -> Expr a
    Sinh :: (Floating a) => Expr a -> Expr a
    Cosh :: (Floating a) => Expr a -> Expr a
    ASinh :: (Floating a) => Expr a -> Expr a
    ACosh :: (Floating a) => Expr a -> Expr a
    ATanh :: (Floating a) => Expr a -> Expr a
    -- Complex
    Re :: Expr (Complex a) -> Expr a
    Im :: Expr (Complex a) -> Expr a

-- | View for unary Expr constructor
viewUnaryOp :: Expr a -> Maybe (Expr a -> Expr a, Expr a)
viewUnaryOp (Abs a) = Just (Abs, a)
viewUnaryOp (Signum a) = Just (Signum, a)
viewUnaryOp (Negate a) = Just (Negate, a)
viewUnaryOp (Exp a) = Just (Exp, a)
viewUnaryOp (Log a) = Just (Log, a)
viewUnaryOp (Sin a) = Just (Sin, a)
viewUnaryOp (Cos a) = Just (Cos, a)
viewUnaryOp (Tan a) = Just (Tan, a)
viewUnaryOp (ASin a) = Just (ASin, a)
viewUnaryOp (ACos a) = Just (ACos, a)
viewUnaryOp (ATan a) = Just (ATan, a)
viewUnaryOp (Sinh a) = Just (Sinh, a)
viewUnaryOp (Cosh a) = Just (Cosh, a)
viewUnaryOp (ASinh a) = Just (ASinh, a)
viewUnaryOp (ACosh a) = Just (ACosh, a)
viewUnaryOp (ATanh a) = Just (ATanh, a)
viewUnaryOp _ = Nothing

-- | view for binary Expr constructor
viewBinaryOp :: Expr a -> Maybe (Expr a -> Expr a -> Expr a, Expr a, Expr a)
viewBinaryOp (a :+: b) = Just ((:+:), a, b)
viewBinaryOp (a :-: b) = Just ((:-:), a, b)
viewBinaryOp (a :*: b) = Just ((:*:), a, b)
viewBinaryOp (a :**: b) = Just ((:**:), a, b)
viewBinaryOp (a :/: b) = Just ((:/:), a, b)
viewBinaryOp _ = Nothing

instance (Show a) => Show (Expr a) where
    show (Const a) = show a
    show (Var name) = name
    show (a :+: b) = show a ++ " + " ++ show b
    show (a :-: b) = show a ++ " - " ++ show b
    show (a :*: b) = show a ++ " * " ++ show b
    show (a :**: b) = show a ++ " ** " ++ show b
    show (a :/: b) = show a ++ " / " ++ show b
    show (Abs a) = '|' : show a ++ "|"
    show (Signum a) = "sgn(" ++ show a ++ ")"
    show (Negate a) = '-' : show a
    show (Exp a) = "exp(" ++ show a ++ ")"
    show (Log a) = "log(" ++ show a ++ ")"
    show (Sin a) = "sin(" ++ show a ++ ")"
    show (Cos a) = "cos(" ++ show a ++ ")"
    show (Tan a) = "tan(" ++ show a ++ ")"
    show (ASin a) = "asin(" ++ show a ++ ")"
    show (ACos a) = "acos(" ++ show a ++ ")"
    show (ATan a) = "atan(" ++ show a ++ ")"
    show (Sinh a) = "sinh(" ++ show a ++ ")"
    show (Cosh a) = "cosh(" ++ show a ++ ")"
    show (ASinh a) = "asinh(" ++ show a ++ ")"
    show (ACosh a) = "acosh(" ++ show a ++ ")"
    show (ATanh a) = "atanh(" ++ show a ++ ")"
    show (Re a) = "Re(" ++ show a ++ ")"
    show (Im a) = "Im(" ++ show a ++ ")"

-- instance (Eq a) => Eq (Expr a) where
--     (Const x) == (Const y) = x == y
--     (Var x) == (Var y) = x == y
--     (viewUnaryOp -> Just (f, x)) == (viewUnaryOp -> Just (g, y)) = getAll $ foldMap All [f == g, x == y]

instance (Num a) => Num (Expr a) where
    fromInteger = Const . fromInteger
    (+) = (:+:)
    (*) = (:+:)
    abs = Abs
    signum = Signum
    negate = Negate

instance (Fractional a) => Fractional (Expr a) where
    fromRational = Const . fromRational
    (/) = (:/:)

instance (Floating a) => Floating (Expr a) where
    pi = Const pi
    exp = Exp
    log = Log
    sin = Sin
    cos = Cos
    asin = ASin
    acos = ACos
    atan = ATan
    sinh = Sinh
    cosh = Cosh
    asinh = ASinh
    acosh = ACosh
    atanh = ATanh

-- | evaluate an expression to the deepest level
eval :: Expr a -> Expr a
eval (Const a) = Const a
eval (Var name) = Var name
eval (Const x :+: Const y) = Const (x + y)
eval (Const x :-: Const y) = Const (x - y)
eval (Const x :*: Const y) = Const (x * y)
eval (Const x :**: Const y) = Const (x ** y)
eval (Const x :/: Const y) = Const (x / y)
eval (Abs (Const a)) = Const (abs a)
eval (Signum (Const a)) = Const (signum a)
eval (Negate (Const a)) = Const (negate a)
eval (Exp (Const a)) = Const (exp a)
eval (Log (Const a)) = Const (log a)
eval (Sin (Const a)) = Const (sin a)
eval (Cos (Const a)) = Const (cos a)
eval (Tan (Const a)) = Const (tan a)
eval (ASin (Const a)) = Const (asin a)
eval (ACos (Const a)) = Const (acos a)
eval (ATan (Const a)) = Const (atan a)
eval (Sinh (Const a)) = Const (sinh a)
eval (Cosh (Const a)) = Const (cosh a)
eval (ASinh (Const a)) = Const (asinh a)
eval (ACosh (Const a)) = Const (acosh a)
eval (ATanh (Const a)) = Const (atanh a)
eval (Re (Const a)) = Const (realPart a)
eval (Im (Const a)) = Const (imagPart a)
eval (viewUnaryOp -> Just (f, a)) = eval $ f (eval a)
eval (viewBinaryOp -> Just (f, a, b)) = eval $ f (eval a) (eval b)
eval _expr = error "failed to evaluate expression"

{- | substitute a value into a variable
with the same name as the string
-}
subVar :: Expr a -> String -> a -> Expr a
subVar (Var x) name val
    | x == name = Const val
    | otherwise = Var x
subVar (Const a) _ _ = Const a
subVar (viewUnaryOp -> Just (f, a)) name val = f (subVar a name val)
subVar (viewBinaryOp -> Just (f, a, b)) name val = f (subVar a name val) (subVar b name val)
subVar e _ _ = e
