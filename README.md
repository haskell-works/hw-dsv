# hw-dsv
[![CircleCI](https://circleci.com/gh/haskell-works/hw-dsv.svg?style=svg)](https://circleci.com/gh/haskell-works/hw-dsv)
[![Travis](https://travis-ci.org/haskell-works/hw-dsv.svg?branch=master)](https://travis-ci.org/haskell-works/hw-dsv)

Unbelievably fast streaming DSV file parser that reads based on succinct data structures.

This library will use support for some BMI2 or AVX2 CPU instructions on some x86 based
CPUs if compiled with the appropriate flags on `ghc-8.4.1` or later.

## Compilation

Pre-requisites:

* Install [Haskell Stack](https://docs.haskellstack.org/en/stable/README/)

It is sufficient to build, test and benchmark the library as follows
for basic performance.  The library will be compiled to use broadword
implementation of rank & select, which has reasonable performance.

```text
stack build
stack test
stack bench
```

For best performance, add the `bmi2` and `avx2` flag to target the BMI2 and AVS2 instruction sets:

```text
stack build   --flag bits-extra:bmi2 --flag hw-rankselect-base:bmi2 --flag hw-rankselect:bmi2 --flag hw-simd:bmi2 --flag hw-simd:avx2 --flag hw-dsv:bmi2 --flag hw-dsv:avx2
stack test    --flag bits-extra:bmi2 --flag hw-rankselect-base:bmi2 --flag hw-rankselect:bmi2 --flag hw-simd:bmi2 --flag hw-simd:avx2 --flag hw-dsv:bmi2 --flag hw-dsv:avx2
stack bench   --flag bits-extra:bmi2 --flag hw-rankselect-base:bmi2 --flag hw-rankselect:bmi2 --flag hw-simd:bmi2 --flag hw-simd:avx2 --flag hw-dsv:bmi2 --flag hw-dsv:avx2
stack install --flag bits-extra:bmi2 --flag hw-rankselect-base:bmi2 --flag hw-rankselect:bmi2 --flag hw-simd:bmi2 --flag hw-simd:avx2 --flag hw-dsv:bmi2 --flag hw-dsv:avx2
```

For slightly older CPUs, add only the `bmi2` flag to target the BMI2 instruction set: 

```text
stack build   --flag bits-extra:bmi2 --flag hw-rankselect-base:bmi2 --flag hw-rankselect:bmi2 --flag hw-simd:bmi2 --flag hw-dsv:bmi2
stack test    --flag bits-extra:bmi2 --flag hw-rankselect-base:bmi2 --flag hw-rankselect:bmi2 --flag hw-simd:bmi2 --flag hw-dsv:bmi2
stack bench   --flag bits-extra:bmi2 --flag hw-rankselect-base:bmi2 --flag hw-rankselect:bmi2 --flag hw-simd:bmi2 --flag hw-dsv:bmi2
stack install --flag bits-extra:bmi2 --flag hw-rankselect-base:bmi2 --flag hw-rankselect:bmi2 --flag hw-simd:bmi2 --flag hw-dsv:bmi2
```

## Benchmark results

The following benchmark shows the kinds of performance gain that can
be expected from enabling the BMI2 instruction set for CPU targets
that support them.  Benchmarks were run on 2.9 GHz Intel Core i7,
macOS High Sierra.

With BMI2 disabled:

```text
$ stack install
$ cat 7g.csv | pv -t -e -b -a | hw-dsv query-lazy -k 0 -k 1 -d , -e '|' > /dev/null
7.08GiB 0:07:25 [16.3MiB/s]
```

With BMI2 and AVX2 enabled:

```text
$ stack install --flag bits-extra:bmi2 --flag hw-bits:bmi2 --flag hw-rankselect-base:bmi2 --flag hw-rankselect:bmi2 --flag hw-dsv:bmi2 --flag hw-dsv:avx2
$ cat 7gb.csv | pv -t -e -b -a | hw-dsv query-lazy -k 0 -k 1 -d , -e '|' > /dev/null
7.08GiB 0:00:39 [ 181MiB/s]
```

With only BMI2 enabled:

```text
$ stack install --flag bits-extra:bmi2 --flag hw-bits:bmi2 --flag hw-rankselect-base:bmi2 --flag hw-rankselect:bmi2 --flag hw-dsv:bmi2
$ cat 7gb.csv | pv -t -e -b -a | hw-dsv query-lazy -k 0 -k 1 -d , -e '|' > /dev/null
7.08GiB 0:00:43 [ 165MiB/s]
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
