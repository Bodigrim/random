{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Data.Int
import Data.Proxy
import Data.Typeable
import Data.Word
import Foreign.C.Types
import Gauge.Main

import System.Random

main :: IO ()
main = do
  let !sz = 100000
  defaultMain
    [ bgroup "pure"
      [ bgroup "random"
        [ pureRandomBench @Float sz
        , pureRandomBench @Double sz
        , pureRandomBench @Integer sz
        , pureRandomBench @Word8 sz
        , pureRandomBench @Word16 sz
        , pureRandomBench @Word32 sz
        , pureRandomBench @Word64 sz
        , pureRandomBench @Word sz
        , pureRandomBench @Int8 sz
        , pureRandomBench @Int16 sz
        , pureRandomBench @Int32 sz
        , pureRandomBench @Int64 sz
        , pureRandomBench @Int sz
        , pureRandomBench @Char sz
        , pureRandomBench @Bool sz
        -- , pureRandomBench @CBool sz
        , pureRandomBench @CChar sz
        , pureRandomBench @CSChar sz
        , pureRandomBench @CUChar sz
        , pureRandomBench @CShort sz
        , pureRandomBench @CUShort sz
        , pureRandomBench @CInt sz
        , pureRandomBench @CUInt sz
        , pureRandomBench @CLong sz
        , pureRandomBench @CULong sz
        , pureRandomBench @CPtrdiff sz
        , pureRandomBench @CSize sz
        , pureRandomBench @CWchar sz
        , pureRandomBench @CSigAtomic sz
        , pureRandomBench @CLLong sz
        , pureRandomBench @CULLong sz
        , pureRandomBench @CIntPtr sz
        , pureRandomBench @CUIntPtr sz
        , pureRandomBench @CIntMax sz
        , pureRandomBench @CUIntMax sz
        ]
      , bgroup "randomR"
        [ bgroup "full"
          [ pureUniformRFullBench @Word8 sz
          , pureUniformRFullBench @Word16 sz
          , pureUniformRFullBench @Word32 sz
          , pureUniformRFullBench @Word64 sz
          , pureUniformRFullBench @Word sz
          , pureUniformRFullBench @Int8 sz
          , pureUniformRFullBench @Int16 sz
          , pureUniformRFullBench @Int32 sz
          , pureUniformRFullBench @Int64 sz
          , pureUniformRFullBench @Int sz
          , pureUniformRFullBench @Char sz
          , pureUniformRFullBench @Bool sz
          -- , pureUniformRFullBench @CBool sz
          , pureUniformRFullBench @CChar sz
          , pureUniformRFullBench @CSChar sz
          , pureUniformRFullBench @CUChar sz
          , pureUniformRFullBench @CShort sz
          , pureUniformRFullBench @CUShort sz
          , pureUniformRFullBench @CInt sz
          , pureUniformRFullBench @CUInt sz
          , pureUniformRFullBench @CLong sz
          , pureUniformRFullBench @CULong sz
          , pureUniformRFullBench @CPtrdiff sz
          , pureUniformRFullBench @CSize sz
          , pureUniformRFullBench @CWchar sz
          , pureUniformRFullBench @CSigAtomic sz
          , pureUniformRFullBench @CLLong sz
          , pureUniformRFullBench @CULLong sz
          , pureUniformRFullBench @CIntPtr sz
          , pureUniformRFullBench @CUIntPtr sz
          , pureUniformRFullBench @CIntMax sz
          , pureUniformRFullBench @CUIntMax sz
          ]
        , bgroup "excludeMax"
          [ pureUniformRExcludeMaxBench @Word8 sz
          , pureUniformRExcludeMaxBench @Word16 sz
          , pureUniformRExcludeMaxBench @Word32 sz
          , pureUniformRExcludeMaxBench @Word64 sz
          , pureUniformRExcludeMaxBench @Word sz
          , pureUniformRExcludeMaxBench @Int8 sz
          , pureUniformRExcludeMaxBench @Int16 sz
          , pureUniformRExcludeMaxBench @Int32 sz
          , pureUniformRExcludeMaxBench @Int64 sz
          , pureUniformRExcludeMaxBench @Int sz
          , pureUniformRExcludeMaxEnumBench @Char sz
          , pureUniformRExcludeMaxEnumBench @Bool sz
          -- , pureUniformRExcludeMaxBench @CBool sz
          , pureUniformRExcludeMaxBench @CChar sz
          , pureUniformRExcludeMaxBench @CSChar sz
          , pureUniformRExcludeMaxBench @CUChar sz
          , pureUniformRExcludeMaxBench @CShort sz
          , pureUniformRExcludeMaxBench @CUShort sz
          , pureUniformRExcludeMaxBench @CInt sz
          , pureUniformRExcludeMaxBench @CUInt sz
          , pureUniformRExcludeMaxBench @CLong sz
          , pureUniformRExcludeMaxBench @CULong sz
          , pureUniformRExcludeMaxBench @CPtrdiff sz
          , pureUniformRExcludeMaxBench @CSize sz
          , pureUniformRExcludeMaxBench @CWchar sz
          , pureUniformRExcludeMaxBench @CSigAtomic sz
          , pureUniformRExcludeMaxBench @CLLong sz
          , pureUniformRExcludeMaxBench @CULLong sz
          , pureUniformRExcludeMaxBench @CIntPtr sz
          , pureUniformRExcludeMaxBench @CUIntPtr sz
          , pureUniformRExcludeMaxBench @CIntMax sz
          , pureUniformRExcludeMaxBench @CUIntMax sz
          ]
        , bgroup "includeHalf"
          [ pureUniformRIncludeHalfBench @Word8 sz
          , pureUniformRIncludeHalfBench @Word16 sz
          , pureUniformRIncludeHalfBench @Word32 sz
          , pureUniformRIncludeHalfBench @Word64 sz
          , pureUniformRIncludeHalfBench @Word sz
          , pureUniformRIncludeHalfBench @Int8 sz
          , pureUniformRIncludeHalfBench @Int16 sz
          , pureUniformRIncludeHalfBench @Int32 sz
          , pureUniformRIncludeHalfBench @Int64 sz
          , pureUniformRIncludeHalfBench @Int sz
          , pureUniformRIncludeHalfEnumBench @Char sz
          , pureUniformRIncludeHalfEnumBench @Bool sz
          -- , pureUniformRIncludeHalfBench @CBool sz
          , pureUniformRIncludeHalfBench @CChar sz
          , pureUniformRIncludeHalfBench @CSChar sz
          , pureUniformRIncludeHalfBench @CUChar sz
          , pureUniformRIncludeHalfBench @CShort sz
          , pureUniformRIncludeHalfBench @CUShort sz
          , pureUniformRIncludeHalfBench @CInt sz
          , pureUniformRIncludeHalfBench @CUInt sz
          , pureUniformRIncludeHalfBench @CLong sz
          , pureUniformRIncludeHalfBench @CULong sz
          , pureUniformRIncludeHalfBench @CPtrdiff sz
          , pureUniformRIncludeHalfBench @CSize sz
          , pureUniformRIncludeHalfBench @CWchar sz
          , pureUniformRIncludeHalfBench @CSigAtomic sz
          , pureUniformRIncludeHalfBench @CLLong sz
          , pureUniformRIncludeHalfBench @CULLong sz
          , pureUniformRIncludeHalfBench @CIntPtr sz
          , pureUniformRIncludeHalfBench @CUIntPtr sz
          , pureUniformRIncludeHalfBench @CIntMax sz
          , pureUniformRIncludeHalfBench @CUIntMax sz
          ]
        , bgroup "unbounded"
          [ pureUniformRBench @Float (1.23e-4, 5.67e8) sz
          , pureUniformRBench @Double (1.23e-4, 5.67e8) sz
          , let !i = (10 :: Integer) ^ (100 :: Integer)
                !range = (-i - 1, i + 1)
            in pureUniformRBench @Integer range sz
          ]
        ]
      ]
    ]

pureRandomBench :: forall a. (Typeable a, Random a) => Int -> Benchmark
pureRandomBench = let !stdGen = mkStdGen 1337 in pureBench @a (genMany (random @a) stdGen)

pureUniformRFullBench :: forall a. (Typeable a, Random a, Bounded a) => Int -> Benchmark
pureUniformRFullBench = let !range = (minBound @a, maxBound @a) in pureUniformRBench range

pureUniformRExcludeMaxBench :: forall a. (Typeable a, Random a, Bounded a, Num a) => Int -> Benchmark
pureUniformRExcludeMaxBench = let !range = (minBound @a, maxBound @a - 1) in pureUniformRBench range

pureUniformRExcludeMaxEnumBench :: forall a. (Typeable a, Random a, Bounded a, Enum a) => Int -> Benchmark
pureUniformRExcludeMaxEnumBench = let !range = (minBound @a, pred (maxBound @a)) in pureUniformRBench range

pureUniformRIncludeHalfBench :: forall a. (Typeable a, Random a, Bounded a, Integral a) => Int -> Benchmark
pureUniformRIncludeHalfBench = let !range = (minBound @a, (maxBound @a `div` 2) + 1) in pureUniformRBench range

pureUniformRIncludeHalfEnumBench :: forall a. (Typeable a, Random a, Bounded a, Enum a) => Int -> Benchmark
pureUniformRIncludeHalfEnumBench =
  let !range = (succ (minBound @a), toEnum ((fromEnum (maxBound @a) `div` 2) + 1))
  in pureUniformRBench range

pureUniformRBench :: forall a. (Typeable a, Random a) => (a, a) -> Int -> Benchmark
pureUniformRBench range =
  let !stdGen = mkStdGen 1337
  in pureBench @a (genMany (randomR range) stdGen)

pureBench :: forall a. (Typeable a) => (Int -> ()) -> Int -> Benchmark
pureBench f sz = bench (showsTypeRep (typeRep (Proxy :: Proxy a)) "") $ nf f sz

genMany :: (g -> (a, g)) -> g -> Int -> ()
genMany f g0 n = go g0 0
  where
    go g i
      | i < n =
        case f g of
          (x, g') -> x `seq` go g' (i + 1)
      | otherwise = g `seq` ()
