{-# OPTIONS_GHC -O2 -ddump-simpl -dsuppress-all -dno-suppress-type-signatures #-}

module Lib (uniformR_Int8) where

import System.Random
import System.Random.Stateful
import Data.Int
import Data.Word

uniformR_Int8 :: (Int8, StdGen)
uniformR_Int8 = uniformR (minBound, maxBound) (mkStdGen 42)
