module Main where

import Criterion.Main (bench, bgroup, defaultMain, whnf)
import Protolude (
  IO,
  Int,
  Maybe (..),
  Num ((+), (-)),
  Ord ((<)),
  otherwise,
  ($),
 )


fib :: Int -> Maybe Int
fib m
  | m < 0 = Nothing
  | otherwise = Just $ go m
  where
    go 0 = 0
    go 1 = 1
    go n = go (n - 1) + go (n - 2)


main :: IO ()
main =
  defaultMain
    [ bgroup
        "fib"
        [ bench "1" $ whnf fib 1
        , bench "5" $ whnf fib 5
        , bench "9" $ whnf fib 9
        , bench "11" $ whnf fib 11
        ]
    ]
