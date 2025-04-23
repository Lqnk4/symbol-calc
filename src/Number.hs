{-# LANGUAGE GADTs #-}
module Number where

import Data.Complex (Complex, realPart)
import Data.Ratio (Ratio)
import GHC.Real ( Ratio(..))
import Data.Function (on)

data Number where
  NumZ :: Integer -> Number
  NumQ :: (Ratio Integer) -> Number
  NumR :: Double -> Number
  NumC :: (Complex Double) -> Number

instance Show Number where
    show (NumZ n) = show n
    show (NumQ q) = show q
    show (NumR x) = show x
    show (NumC z) = show z

instance Eq Number where
    (NumZ n1) == (NumZ n2) = n1 == n2
    (NumQ q1) == (NumQ q2) = q1 == q2
    (NumR x1) == (NumR x2) = x1 == x2
    (NumC z1) == (NumC z2) = z1 == z2


toZ :: Number -> Number
toZ n@(NumZ _) = n
toZ (NumQ (x :% y)) = NumZ $ if y == 1 then x else round (( fromIntegral x / fromIntegral y) :: Double)
toZ (NumR x) = NumZ . round $ x
toZ (NumC z) = NumZ . round . realPart $ z
