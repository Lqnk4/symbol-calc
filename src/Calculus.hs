{-# LANGUAGE DataKinds #-}
module Calculus (
    derive,
) where

import Expression


derive :: (Floating a) => Expr a ->  Expr a-> Expr a
derive = undefined
