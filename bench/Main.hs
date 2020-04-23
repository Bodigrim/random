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
  let !sz = 1000000
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
      ]
    ]

pureRandomBench :: forall a. (Typeable a, Random a) => Int -> Benchmark
pureRandomBench = let !stdGen = mkStdGen 1337 in pureBench @a (genManyRandom @a stdGen)

pureBench :: forall a. (Typeable a) => (Int -> ()) -> Int -> Benchmark
pureBench f sz = bench (showsTypeRep (typeRep (Proxy :: Proxy a)) "") $ nf f sz

genManyRandom :: forall a g. (Random a, RandomGen g) => g -> Int -> ()
genManyRandom = genMany (random @a)

genMany :: (g -> (a, g)) -> g -> Int -> ()
genMany f g0 n = go g0 0
  where
    go g i
      | i < n =
        case f g of
          (x, g') -> x `seq` go g' (i + 1)
      | otherwise = g `seq` ()
