# hw-dsv
[![CircleCI](https://circleci.com/gh/haskell-works/hw-dsv.svg?style=svg)](https://circleci.com/gh/haskell-works/hw-dsv)

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
$ time stack exec -- hw-dsv query --file corpus/medium.csv --create-index --column 0 --column 2 --column 4 --column 6 --column 8 --column 20 --column 22 --delimiter , > /dev/null
stack exec -- hw-dsv query --file corpus/medium.csv --create-index --column 0   184.04s user 15.66s system 137% cpu 2:25.67 total
```

With BMI2 enabled:

```text
$ stack build --flag bits-extra:bmi2 --flag hw-rankselect-base:bmi2 --flag hw-rankselect:bmi2 --flag hw-dsv:bmi2
$ time stack exec -- hw-dsv query --file corpus/medium.csv --create-index --column 0 --column 2 --column 4 --column 6 --column 8 --column 20 --column 22 --delimiter , > /dev/null
stack exec -- hw-dsv query --file corpus/medium.csv --create-index --column 0   97.60s user 13.96s system 182% cpu 1:01.03 total
```

## References

* [Space-Efficient, High-Performance Rank & Select Structures on Uncompressed Bit Sequences](http://www.cs.cmu.edu/~./dga/papers/zhou-sea2013.pdf)
