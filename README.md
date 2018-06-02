# hw-dsv
[![CircleCI](https://circleci.com/gh/haskell-works/hw-dsv.svg?style=svg)](https://circleci.com/gh/haskell-works/hw-dsv)
[![Travis](https://travis-ci.org/haskell-works/hw-dsv.svg?branch=master)](https://travis-ci.org/haskell-works/hw-dsv)

Unbelievably fast DSV file parser that readers based on succinct data structures.

This library will use support for some BMI2 CPU instructions on some x86 based
CPUs if compiled with the appropriate flags on `ghc-8.4.1` or later.

## Compilation

It is sufficient to build, test and benchmark the library as follows
for basic performance.  The library will be compiled to use broadword
implementation of rank & select, which has reasonable performance.

```text
stack build
stack test
stack bench
```

For best perform, add the `bmi2` flag to target the BMI2 instruction set:

```text
stack build --flag bits-extra:bmi2 --flag hw-rankselect-base:bmi2 --flag hw-rankselect:bmi2 --flag hw-dsv:bmi2
stack test  --flag bits-extra:bmi2 --flag hw-rankselect-base:bmi2 --flag hw-rankselect:bmi2 --flag hw-dsv:bmi2
stack bench --flag bits-extra:bmi2 --flag hw-rankselect-base:bmi2 --flag hw-rankselect:bmi2 --flag hw-dsv:bmi2
```

## Benchmark results

The following benchmark shows the kinds of performance gain that can
be expected from enabling the BMI2 instruction set for CPU targets
that support them.  Benchmarks were run on 2.9 GHz Intel Core i7,
macOS High Sierra.

With BMI2 disabled:

```text
$ stack build
cat corpus/medium.csv | time stack exec -- hw-dsv query-lazy -k 0 -k 2 -k 4 -k 6 -k 8 -k 20 -k 22 --delimiter , --out-delimiter , > /dev/null
39.15s user 5.79s system 184% cpu 24.325 total
```

With BMI2 enabled:

```text
$ stack build --flag bits-extra:bmi2 --flag hw-rankselect-base:bmi2 --flag hw-rankselect:bmi2 --flag hw-dsv:bmi2
$ cat corpus/medium.csv | time stack exec -- hw-dsv query-lazy -k 0 -k 2 -k 4 -k 6 -k 8 -k 20 -k 22 --delimiter , --out-delimiter , > /dev/null
144.53s user 11.33s system 131% cpu 1:58.88 total
```

## Using `hw-dsv` as a library

```haskell
{-# LANGUAGE ScopedTypeVariables #-}

module Example where

import qualified Data.ByteString.Lazy              as LBS
import qualified Data.Vector                       as DV
import qualified HaskellWorks.Data.Dsv.Lazy.Cursor as SVL

example :: IO ()
example = do
  bs <- LBS.readFile "sample.csv"
  let c = SVL.makeCursor ',' bs
  let rows :: [DV.Vector LBS.ByteString] = SVL.toListVector c

  return ()
```
