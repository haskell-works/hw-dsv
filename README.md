# hw-dsv
[![CircleCI](https://circleci.com/gh/haskell-works/hw-dsv.svg?style=svg)](https://circleci.com/gh/haskell-works/hw-dsv)
[![Travis](https://travis-ci.org/haskell-works/hw-dsv.svg?branch=master)](https://travis-ci.org/haskell-works/hw-dsv)

Unbelievably fast streaming DSV file parser that reads based on succinct data structures.

This library will use support for some BMI2 or AVX2 CPU instructions on some x86 based
CPUs if compiled with the appropriate flags on `ghc-8.4.1` or later.

## Compilation & Installation

Pre-requisites:

* `cabal-install-3.0.0.0`
* `ghc-8.4.4` or higher

It is sufficient to build, test and benchmark the library as follows
for basic performance.  The library will be compiled to use broadword
implementation of rank & select, which has reasonable performance.

```bash
cabal v2-configure --enable-tests --enable-benchmarks --disable-documentation
cabal v2-build
cabal v2-test
cabal v2-bench
cabal v2-install --overwrite-policy=always --installdir="$HOME/.local/bin"
```

Ensure that `$HOME/.local/bin` is in your path if you are using intending to
use the `hw-dsv` binary.

For best performance, add the `bmi2` and `avx2` flags to target the BMI2 and
AVX2 instruction are specified in the `cabal.project` file.

For slightly older CPUs, remove `avx2` flags from the `cabal.project` file to
target only the BMI2 instruction set.

### Stack support

It should be possible to install `hw-dsv` via stack:

```bash
stack install --flag bits-extra:bmi2 --flag hw-rankselect-base:bmi2 --flag hw-rankselect:bmi2 --flag hw-simd:bmi2 --flag hw-simd:avx2 --flag hw-dsv:bmi2 --flag hw-dsv:avx2
```

Although your mileage may vary depending on which snapshot you are using.

The flags should be adjusted for the CPU you are targetting.

## Benchmark results

The following benchmark shows the kinds of performance gain that can
be expected from enabling the BMI2 instruction set for CPU targets
that support them.  Benchmarks were run on 2.9 GHz Intel Core i7,
macOS High Sierra.

With BMI2 disabled:

```bash
$ cat 7g.csv | pv -t -e -b -a | hw-dsv query-lazy -k 1 -k 2 -d , -e '|' > /dev/null
7.08GiB 0:07:25 [16.3MiB/s]
```

With BMI2 and AVX2 enabled:

```bash
$ cat 7gb.csv | pv -t -e -b -a | hw-dsv query-lazy -k 1 -k 2 -d , -e '|' > /dev/null
7.08GiB 0:00:39 [ 181MiB/s]
```

With only BMI2 enabled:

```bash
$ cat 7gb.csv | pv -t -e -b -a | hw-dsv query-lazy -k 1 -k 2 -d , -e '|' > /dev/null
7.08GiB 0:00:43 [ 165MiB/s]
```

## `hw-dsv` command line options

The `hw-dsv` application accepts 1-based column indexes rather than 0-based. The library is 0-based.

## Using `hw-dsv` as a library

```haskell
{-# LANGUAGE ScopedTypeVariables #-}

module Example where

import qualified Data.ByteString.Lazy                   as LBS
import qualified Data.Vector                            as DV
import qualified HaskellWorks.Data.Dsv.Lazy.Cursor      as SVL
import qualified HaskellWorks.Data.Dsv.Lazy.Cursor.Lazy as SVL

example :: IO ()
example = do
  bs <- LBS.readFile "sample.csv"
  let c = SVL.makeCursor ',' bs
  let rows :: [DV.Vector LBS.ByteString] = SVL.toListVector c

  return ()
```
